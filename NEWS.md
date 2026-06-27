# graphfast 0.2.0

## Scope change

* The package is now focused exclusively on graph analysis and graph-based
  entity resolution. The standalone string-matching utilities have been
  **removed**: `multi_grepl()`, `filter_strings()`, and the `%fgrepl%` /
  `%fgrepli%` infix operators (and their C++ backends). Base-R `grepl()` covers
  the few remaining fixed-string needs, and packages such as `stringi` are a
  better home for general string matching.

## Performance

* `UnionFind::find()` is now iterative with path-halving, removing the
  recursion-depth (stack overflow) risk and improving cache behaviour on graphs
  with hundreds of millions of edges.
* `find_connected_components()` and `get_edge_components()` relabel component
  roots with an O(n) vector lookup instead of `std::map` (O(n log n) plus
  per-node tree allocations).
* `shortest_paths()` no longer reallocates and re-zeroes an `O(n_nodes)` distance
  buffer for every query. A single buffer is reused with a visited-version stamp,
  and the adjacency list is pre-reserved to each node's exact degree. This is a
  large speed-up when answering many queries against one graph.
* `find_connected_components_safe()` and `find_connected_components_large()` now
  remap node IDs with `fastmatch::fmatch()` on the numeric IDs instead of an
  `as.character()` + named-vector lookup, which was slow and memory-heavy for
  millions of nodes.
* `find_connected_components()` and `get_edge_components()` defer the expensive
  `unique()` node scan so it only runs when a memory warning actually needs it.

## Bug fixes

* `find_connected_components(compress = FALSE)` now returns the correct
  `n_components` and a populated `component_sizes` vector (previously `0` and an
  empty vector).

## Other

* `find_connected_components_large()` gains a `verbose` argument; its progress
  messages are now silent by default.
