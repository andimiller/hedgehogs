package net.andimiller.hedgehogs.dag.visitor

import cats.Applicative
import cats.implicits._
import cats.effect.implicits.genSpawnOps
import cats.effect.{Concurrent, Deferred, Fiber, Ref, Resource}
import cats.effect.std.{Queue, Semaphore}
import net.andimiller.hedgehogs.{Dag, DataGraph}
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.{RunMode, SubtaskFailed}

/** The result of visiting a single node: either an output, or a suspension with a handle that lets the visitor
  * reconnect to some slow external work later.
  */
sealed trait StepResult[+Out, +Wait]
object StepResult {
  case class Complete[Out](output: Out)  extends StepResult[Out, Nothing]
  case class Suspend[Wait](handle: Wait) extends StepResult[Nothing, Wait]
}

/** The progressive state of a node during a suspendable run.
  *
  * There is deliberately no `Running`: a running fiber doesn't survive a process restart, so after rehydration such a
  * node must be run again from scratch — which is what `Pending` already means.
  */
sealed trait NodeState[+In, +Out, +Wait]
object NodeState {

  /** Not run yet */
  case class Pending[In](input: In) extends NodeState[In, Nothing, Nothing]

  /** Ran and suspended; blocks downstream until resumed with an external payload */
  case class Waiting[In, Wait](input: In, handle: Wait) extends NodeState[In, Nothing, Wait]

  /** Has an output */
  case class Done[Out](output: Out) extends NodeState[Nothing, Out, Nothing]
}

/** A serializable picture of a suspended run: the progressive graph is the whole run state. */
case class DagSnapshot[Id, In, Out, Wait, Edge](
    graph: DataGraph[Id, NodeState[In, Out, Wait], Edge],
    direction: RunMode,
    version: Int = DagSnapshot.CurrentVersion
)
object DagSnapshot {
  val CurrentVersion: Int = 1
}

sealed trait RunResult[Id, In, Out, Wait, Edge]
object RunResult {
  case class Finished[Id, In, Out, Wait, Edge](graph: DataGraph[Id, Out, Edge])
      extends RunResult[Id, In, Out, Wait, Edge]
  case class Suspended[Id, In, Out, Wait, Edge](snapshot: DagSnapshot[Id, In, Out, Wait, Edge])
      extends RunResult[Id, In, Out, Wait, Edge]
}

/** Lifecycle events emitted by the runner, for logging/metrics. Pass a callback to `start`/`resume` to receive them;
  * events for a node run inside that node's task, so a callback that raises will fail the node like any other step
  * error — keep callbacks cheap and total.
  */
sealed trait RunEvent[+Id, +Wait]
object RunEvent {

  /** a node's first visit has been scheduled */
  case class NodeStarted[Id](id: Id) extends RunEvent[Id, Nothing]

  /** a node kicked off external work and is now waiting on this handle */
  case class NodeSuspended[Id, Wait](id: Id, handle: Wait) extends RunEvent[Id, Wait]

  /** a waiting node has been given its external payload and is resuming */
  case class NodeResumed[Id, Wait](id: Id, handle: Wait) extends RunEvent[Id, Wait]

  /** a node finished with an output */
  case class NodeCompleted[Id](id: Id) extends RunEvent[Id, Nothing]

  def noop[F[_]: Applicative, Id, Wait]: RunEvent[Id, Wait] => F[Unit] = _ => Applicative[F].unit
}

trait SimpleSuspendableDagVisitor[F[_], Id, In, Out, Edge, Wait, Payload]
    extends SuspendableDagVisitor[F, Id, In, Out, Edge, Wait, Payload] {
  def run(id: Id, node: In, inputs: Map[Id, Out]): F[StepResult[Out, Wait]]
  def resume(id: Id, node: In, handle: Wait, payload: Payload, inputs: Map[Id, Out]): F[StepResult[Out, Wait]]

  override def run(
      id: Id,
      node: In,
      graph: DataGraph[Id, NodeState[In, Out, Wait], Edge],
      inputs: Map[(Id, Edge), Out]
  ): F[StepResult[Out, Wait]] =
    run(id, node, inputs.map { case ((k, _), v) => k -> v })

  override def resume(
      id: Id,
      node: In,
      handle: Wait,
      payload: Payload,
      graph: DataGraph[Id, NodeState[In, Out, Wait], Edge],
      inputs: Map[(Id, Edge), Out]
  ): F[StepResult[Out, Wait]] =
    resume(id, node, handle, payload, inputs.map { case ((k, _), v) => k -> v })
}

trait SuspendableDagVisitor[F[_], Id, In, Out, Edge, Wait, Payload] {

  /** First visit: do the work, or kick off something slow and suspend with a handle. */
  def run(
      id: Id,
      node: In,
      graph: DataGraph[Id, NodeState[In, Out, Wait], Edge],
      inputs: Map[(Id, Edge), Out]
  ): F[StepResult[Out, Wait]]

  /** Called when a Waiting node is given an external payload; may complete or suspend again. */
  def resume(
      id: Id,
      node: In,
      handle: Wait,
      payload: Payload,
      graph: DataGraph[Id, NodeState[In, Out, Wait], Edge],
      inputs: Map[(Id, Edge), Out]
  ): F[StepResult[Out, Wait]]
}

object SuspendableDagVisitor {

  case class InvalidDag(message: String) extends Throwable {
    override def getMessage: String = message
  }

  case class InvalidCompletions(message: String) extends Throwable {
    override def getMessage: String = message
  }

  /** Raised if a run stops making progress with Pending nodes left but nothing Waiting; unreachable for validated DAGs,
    * kept as a guard against malformed snapshots.
    */
  case class StuckDag(message: String) extends Throwable {
    override def getMessage: String = message
  }

  def start[F[_]: Concurrent, Id, In, Out, Edge, Wait, Payload](
      visitor: SuspendableDagVisitor[F, Id, In, Out, Edge, Wait, Payload],
      direction: RunMode = RunMode.Flow
  )(
      initialDag: DataGraph[Id, In, Edge]
  ): F[RunResult[Id, In, Out, Wait, Edge]] =
    start(visitor, direction, RunEvent.noop[F, Id, Wait])(initialDag)

  def start[F[_]: Concurrent, Id, In, Out, Edge, Wait, Payload](
      visitor: SuspendableDagVisitor[F, Id, In, Out, Edge, Wait, Payload],
      direction: RunMode,
      events: RunEvent[Id, Wait] => F[Unit]
  )(
      initialDag: DataGraph[Id, In, Edge]
  ): F[RunResult[Id, In, Out, Wait, Edge]] =
    validated(initialDag) *>
      runSegment(
        visitor,
        direction,
        initialDag.mapNode(in => NodeState.Pending(in): NodeState[In, Out, Wait]),
        Map.empty[Id, Payload],
        events
      )

  def resume[F[_]: Concurrent, Id, In, Out, Edge, Wait, Payload](
      visitor: SuspendableDagVisitor[F, Id, In, Out, Edge, Wait, Payload]
  )(
      snapshot: DagSnapshot[Id, In, Out, Wait, Edge],
      completions: Map[Id, Payload]
  ): F[RunResult[Id, In, Out, Wait, Edge]] =
    resume(visitor, RunEvent.noop[F, Id, Wait])(snapshot, completions)

  def resume[F[_]: Concurrent, Id, In, Out, Edge, Wait, Payload](
      visitor: SuspendableDagVisitor[F, Id, In, Out, Edge, Wait, Payload],
      events: RunEvent[Id, Wait] => F[Unit]
  )(
      snapshot: DagSnapshot[Id, In, Out, Wait, Edge],
      completions: Map[Id, Payload]
  ): F[RunResult[Id, In, Out, Wait, Edge]] = {
    val badCompletions = completions.keySet.toList.flatMap { id =>
      snapshot.graph.nodeMap.get(id) match {
        case Some(NodeState.Waiting(_, _)) => None
        case Some(_)                       => Some(s"$id is not Waiting")
        case None                          => Some(s"$id is not in the graph")
      }
    }
    (if (badCompletions.nonEmpty) {
       (InvalidCompletions(badCompletions.mkString(", ")): Throwable).raiseError[F, Unit]
     } else ().pure[F]) *>
      validated(snapshot.graph) *>
      runSegment(visitor, snapshot.direction, snapshot.graph, completions, events)
  }

  private def validated[F[_]: Concurrent, Id](g: DataGraph[Id, _, _]): F[Unit] =
    Dag.validate(g).value match {
      case Left(err) => (InvalidDag(err): Throwable).raiseError[F, Unit]
      case Right(()) => ().pure[F]
    }

  private def runSegment[F[_]: Concurrent, Id, In, Out, Edge, Wait, Payload](
      visitor: SuspendableDagVisitor[F, Id, In, Out, Edge, Wait, Payload],
      direction: RunMode,
      initial: DataGraph[Id, NodeState[In, Out, Wait], Edge],
      completions: Map[Id, Payload],
      events: RunEvent[Id, Wait] => F[Unit]
  ): F[RunResult[Id, In, Out, Wait, Edge]] = for {
    completed       <- Queue.unbounded[F, Id] // we'll push to this each time a task finishes
    stateGraph      <- Ref.of[F, DataGraph[Id, NodeState[In, Out, Wait], Edge]](initial)
    errored         <- Deferred[F, SubtaskFailed]
    startedSet      <- Ref.of[F, Set[Id]](Set.empty[Id])
    backgroundTasks <- Ref.of[F, Map[Id, Fiber[F, Throwable, Unit]]](Map.empty)
    tickLock        <- Semaphore[F](1) // we lock this when doing stuff
    isDone           = (s: NodeState[In, Out, Wait]) =>
                         s match {
                           case NodeState.Done(_) => true
                           case _                 => false
                         }
    tick             = tickLock.permit.use { _ =>
                         for {
                           // if there's an error we need to stop early
                           err       <- errored.tryGet
                           _         <- err match {
                                          case Some(e) => e.raiseError[F, Unit]
                                          case _       => ().pure[F]
                                        }
                           // otherwise we can do normal stuff
                           graph     <- stateGraph.get
                           started   <- startedSet.get
                           getInbound = direction match {
                                          case RunMode.Flow       => graph.inbound _
                                          case RunMode.Dependency => graph.outgoing _
                                        }
                           runnable   = graph.nodes
                                          .filterNot(started)
                                          .filter { id =>
                                            graph.nodeMap(id) match {
                                              case NodeState.Pending(_)    => true
                                              case NodeState.Waiting(_, _) => completions.contains(id)
                                              case NodeState.Done(_)       => false
                                            }
                                          }
                                          .filter { id =>
                                            getInbound(id).forall { id2 =>
                                              isDone(graph.nodeMap(id2))
                                            } // all inbounds must be done
                                          }
                           _         <- runnable.toList.traverse { id =>
                                          val inputs = {
                                            getInbound(id)
                                              .map { from => from -> graph.nodeMap(from) }
                                              .collect { case (from, NodeState.Done(r)) =>
                                                direction match {
                                                  case RunMode.Flow       =>
                                                    (from, graph.edgeMap((from, id))) -> r
                                                  case RunMode.Dependency =>
                                                    (from, graph.edgeMap((id, from))) -> r
                                                }
                                              }
                                              .toMap
                                          }
                                          val state    = graph.nodeMap(id)
                                          val announce = state match {
                                            case NodeState.Pending(_)         =>
                                              events(RunEvent.NodeStarted(id))
                                            case NodeState.Waiting(_, handle) =>
                                              events(RunEvent.NodeResumed(id, handle))
                                            case NodeState.Done(_)            =>
                                              ().pure[F]
                                          }
                                          val step     = state match {
                                            case NodeState.Pending(input)         =>
                                              visitor.run(id, input, graph, inputs)
                                            case NodeState.Waiting(input, handle) =>
                                              visitor.resume(id, input, handle, completions(id), graph, inputs)
                                            case NodeState.Done(_)                =>
                                              (new IllegalStateException(s"Done node $id was scheduled"): Throwable)
                                                .raiseError[F, StepResult[Out, Wait]]
                                          }
                                          val task     =
                                            (announce *> step)
                                              .flatMap { result =>
                                                val newState: NodeState[In, Out, Wait] = (result, state) match {
                                                  case (StepResult.Complete(out), _)                        =>
                                                    NodeState.Done(out)
                                                  case (StepResult.Suspend(h), NodeState.Pending(input))    =>
                                                    NodeState.Waiting(input, h)
                                                  case (StepResult.Suspend(h), NodeState.Waiting(input, _)) =>
                                                    NodeState.Waiting(input, h)
                                                  case (StepResult.Suspend(_), NodeState.Done(out))         =>
                                                    NodeState.Done(out) // unreachable, Done is never scheduled
                                                }
                                                val event                              = result match {
                                                  case StepResult.Complete(_) => events(RunEvent.NodeCompleted(id))
                                                  case StepResult.Suspend(h)  => events(RunEvent.NodeSuspended(id, h))
                                                }
                                                stateGraph
                                                  .update {
                                                    _.addNode(id, newState)
                                                  }
                                                  .flatTap { _ =>
                                                    event *>
                                                      completed.offer(id) // tell the queue we're done
                                                  }
                                              }
                                              .onError { case e =>
                                                // if something goes wrong, set the error and then send the complete
                                                errored.complete(SubtaskFailed(id.toString, e)) *>
                                                  completed.offer(id)
                                              }
                                          startedSet.update(_ + id) *>
                                            task.start.flatMap { fibre =>
                                              backgroundTasks.update(_.updated(id, fibre))
                                            }.void
                                        }
                         } yield runnable.size // how many tasks this tick started
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
          // every started task offers to `completed` exactly once, so counting
          // spawned-minus-taken locally tells us when nothing is left in flight;
          // a tick starts everything runnable, so at zero the run cannot make
          // any more progress
          def drive(outstanding: Int): F[Unit] =
            if (outstanding <= 0) ().pure[F]
            else
              completed.take *> tick.flatMap { spawned =>
                drive(outstanding - 1 + spawned)
              }

          tick.flatMap(drive) *>
            // Join each fiber once so transformer state (e.g. WriterT logs) is sequenced
            // into the parent. Errors are already routed via `errored`, so swallow here.
            backgroundTasks.get
              .flatMap(_.values.toList.traverse_(_.joinWithUnit.handleError(_ => ()))) *>
            // if the failing task was the last one in flight the loop exits without ticking,
            // so re-check the error here
            errored.tryGet.flatMap {
              case Some(e) => e.raiseError[F, Unit]
              case None    => ().pure[F]
            }
        }
    graph           <- stateGraph.get
    result          <- {
      val states = graph.nodeMap.values
      if (states.forall(isDone)) {
        (RunResult.Finished(graph.mapNode {
          case NodeState.Done(out) => out
          case other               => throw new IllegalStateException(s"Unexpected state in finished run: $other")
        }): RunResult[Id, In, Out, Wait, Edge]).pure[F]
      } else if (states.exists { case NodeState.Waiting(_, _) => true; case _ => false }) {
        (RunResult.Suspended(DagSnapshot(graph, direction)): RunResult[Id, In, Out, Wait, Edge]).pure[F]
      } else {
        val pending = graph.nodeMap.collect { case (id, NodeState.Pending(_)) => id }
        (StuckDag(s"Run stopped with pending nodes and nothing waiting: ${pending.mkString(",")}"): Throwable)
          .raiseError[F, RunResult[Id, In, Out, Wait, Edge]]
      }
    }
  } yield result

}
