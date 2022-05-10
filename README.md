# hedgehogs

A graph traversal library for cyclic graphs with weighted edges.

Scala Versions: 2.13, 3
Scala Targets: JVM, JS

Features:
* generic `Node`, `Edge` and `Graph` implementation
* `circe` module for deserializing `Node`s and `Edge`s from JSON
* `Dijkstra` implementation for routefinding
* `Dijkstra.multi` implementation for finding multiple routes from one origin in one pass
