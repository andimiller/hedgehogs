# hedgehogs

A generic Graph library for Scala.

Scala Versions: 2.13, 3
Scala Targets: JVM, JS

# Core

## `SimpleGraph[Id]`

* Models a graph with directed edges
* `inbound` and `outbound` methods for any node

## `DataGraph[Id, NodeData, EdgeData]`

* Models a graph with directed edges, and can hold data in the nodes and edges
* `inbound` and `outbound` methods for any node

## `Dijkstra`

* Supports routefinding across any `SimpleGraph` or `DataGraph`
* Supports finding multiple routes at once with `multi` variant

## `Dag`

* `validate` - check if any `SimpleGraph` or `DataGraph` is a Directed Acyclic Graph, flags any nodes with cycles
* `isDag` - checks if a graph is a DAG, returns a Boolean

## `Connectivity`

* `countDisconnectedSubgraphs` - checks how many disconnected subgraphs are in a graph


# Mermaid

## `flowchart`

* Can render any `DataGraph` which is a DAG into a mermaid graph

# Dag Visitor

## `DagVisitor[F[_], Id, InputNodeData, OutputNodeData, EdgeData]`

* Interface which can be implemented to describe how to run a `DataGraph`

## `DagVisitor.runConcurrent`

* Runs a `DagVisitor` against a compatible `DataGraph`
* Runs concurrently, starts nodes as soon as they can be run
* Can run different `RunMode`s, indicating the direction edges run
  * `Flow` will make `A->B` run `A` then `B`, this is the default
  * `Dependency` will make `A->B` run `B` then `A`

## `SuspendableDagVisitor[F[_], Id, In, Out, Edge, Wait, Payload]`

* Like `DagVisitor`, but a node may return `StepResult.Suspend(handle)` instead of an
  output — for steps that kick off slow external work (a remote job, a human approval)
  that shouldn't hold a process alive
* `SuspendableDagVisitor.start` runs the DAG until it either finishes
  (`RunResult.Finished`, with the output graph) or can't make any more progress
  (`RunResult.Suspended`, with a `DagSnapshot` — a serializable picture of the run:
  every node is `Pending`, `Waiting` on a handle, or `Done`)
* Persist the snapshot wherever you like (a database, probably); when the external work
  completes, rehydrate it and call `SuspendableDagVisitor.resume(visitor)(snapshot,
  Map(nodeId -> payload))` — the visitor's `resume` method turns the payload into an
  output (or suspends again), and the run carries on until `Finished` or the next
  `Suspended`
* Steps may be re-run if a segment is retried, so they should be idempotent or
  tolerably re-runnable; single-writer-per-run is the application's responsibility
* Optionally takes a `RunEvent => F[Unit]` callback for logging/metrics — emits
  `NodeStarted`, `NodeSuspended`, `NodeResumed`, and `NodeCompleted` events

# Dag Visitor Circe

## `DagSnapshotCodecs`

* circe `Encoder`/`Decoder` for `DagSnapshot`, given codecs for your id, node, wait
  handle, and edge types — so a suspended run can be stored as JSON and rehydrated later
* See `DagSnapshotCodecsSpec` for the full suspend → persist → rehydrate → resume flow,
  or `examples/suspendable-http` for a runnable demo with an http server and disk-backed
  JSON storage (`sbt exampleSuspendableHttp/run`)
