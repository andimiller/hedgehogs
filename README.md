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
