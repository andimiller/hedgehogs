package net.andimiller.hedgehogs.dag.visitor

import cats.implicits._
import cats.Monad
import cats.effect.implicits.genSpawnOps
import cats.effect.{Concurrent, Deferred, Fiber, Ref, Resource}
import net.andimiller.hedgehogs.DataGraph
import cats.effect.std.{Queue, Semaphore}

trait SimpleDagVisitor[F[_], Id, InputData, OutputData, EdgeData]
    extends DagVisitor[F, Id, InputData, OutputData, EdgeData] {
  def run(id: Id, node: InputData, inputs: Map[Id, OutputData]): F[OutputData]

  override def run(
      id: Id,
      node: InputData,
      graph: DataGraph[Id, Either[InputData, OutputData], EdgeData],
      inputs: Map[(Id, EdgeData), OutputData]
  ): F[OutputData] =
    run(id, node, inputs.map { case ((k, _), v) => k -> v })
}

trait DagVisitor[F[_], Id, InputData, OutputData, EdgeData] {
  def run(
      id: Id,
      node: InputData,
      graph: DataGraph[Id, Either[InputData, OutputData], EdgeData],
      inputs: Map[(Id, EdgeData), OutputData]
  ): F[OutputData]
}

object DagVisitor {

  // different ways you can run a DAG
  sealed trait RunMode
  object RunMode {
    case object Flow       extends RunMode // A->B runs A then B
    case object Dependency extends RunMode // A->B runs B then A
  }

  case class SubtaskFailed(nodeId: String, t: Throwable) extends Throwable {
    override def getMessage: String  = s"Node $nodeId failed to run: ${t.getMessage}"
    override def getCause: Throwable = t
  }

  def runConcurrent[F[_]: Concurrent: Monad, Id, InputData, OutputData, EdgeData](
      visitor: DagVisitor[F, Id, InputData, OutputData, EdgeData],
      direction: RunMode = RunMode.Flow
  )(
      initialDag: DataGraph[Id, InputData, EdgeData]
  ): F[DataGraph[Id, OutputData, EdgeData]] = for {
    completed       <- Queue.unbounded[F, Id] // we'll push to this each time a task finishes
    combinedGraph   <- Ref.of[F, DataGraph[Id, Either[InputData, OutputData], EdgeData]](
                         initialDag.mapNode(_.asLeft)
                       )
    errored         <- Deferred[F, SubtaskFailed]
    startedSet      <- Ref.of[F, Set[Id]](Set.empty[Id])
    backgroundTasks <- Ref.of[F, Map[Id, Fiber[F, Throwable, Unit]]](Map.empty)
    tickLock        <- Semaphore[F](1) // we lock this when doing stuff
    tick             = tickLock.permit.use { _ =>
                         for {
                           // if there's an error we need to stop early
                           err       <- errored.tryGet
                           _         <- err match {
                                          case Some(e) => e.raiseError[F, Unit]
                                          case _       => ().pure[F]
                                        }
                           // otherwise we can do normal stuff
                           graph     <- combinedGraph.get
                           started   <- startedSet.get
                           getInbound = direction match {
                                          case RunMode.Flow       => graph.inbound _
                                          case RunMode.Dependency => graph.outgoing _
                                        }
                           runnable   = graph.nodes
                                          .filterNot(started)
                                          .filter { id => graph.nodeMap(id).isLeft } // we can only run lefts
                                          .filter { id =>
                                            getInbound(id).forall { id2 =>
                                              graph.nodeMap(id2).isRight
                                            } // all inbounds must be rights
                                          }
                           _         <- runnable.toList.traverse { id =>
                                          val inputs = {
                                            getInbound(id)
                                              .map { from => from -> graph.nodeMap(from) }
                                              .collect { case (from, Right(r)) =>
                                                direction match {
                                                  case RunMode.Flow       =>
                                                    (from, graph.edgeMap((from, id))) -> r
                                                  case RunMode.Dependency =>
                                                    (from, graph.edgeMap((id, from))) -> r
                                                }
                                              }
                                              .toMap
                                          }
                                        val task =
                                          visitor
                                            .run(id, graph.nodeMap(id).left.toOption.get, graph, inputs)
                                            .flatMap { result =>
                                              combinedGraph
                                                .update {
                                                  _.addNode(id, Right(result))
                                                }
                                                .flatTap { _ =>
                                                  completed.offer(id) // tell the queue we're done
                                                }
                                            }
                                            .onError { case e =>
                                              // if something goes wrong, set the error and then send the complete
                                              errored.complete(SubtaskFailed(id.toString, e)) *> completed.offer(id)
                                            }
                                        startedSet.update(_ + id) *>
                                          task.start.flatMap { fibre =>
                                            backgroundTasks.update(_.updated(id, fibre))
                                          }.void // we could track these fibres for clean shutdowns, TODO
                                        }
                         } yield ()
                       }
    _               <-
      Resource
        .onFinalize[F] {
          backgroundTasks.get
            .flatMap(_.toList.traverse { case (_, fibre) =>
              fibre.cancel
            })
            .void
        }
        .use { _ =>
          (tick *> (completed.take >> tick)
            .whileM_(combinedGraph.get.map(_.nodeMap.values.exists(_.isLeft))))
        }
    result          <- combinedGraph.get
  } yield result.mapNode { _.toOption.get } // everything should be right now

}
