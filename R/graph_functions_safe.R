#' Memory-Efficient Connected Components with Automatic Node Remapping
#'
#' This version automatically handles sparse large node IDs by mapping them to 
#' consecutive small integers, preventing memory allocation errors.
#'
#' @param edges A two-column matrix or data.frame of edges
#' @param compress Whether to compress component IDs. Default TRUE.
#' @param verbose Whether to print memory usage info. Default TRUE.
#'
#' @return List with components, component_sizes, n_components, and node_mapping
#' @export
find_connected_components_safe <- function(edges, compress = TRUE, verbose = TRUE) {
  # Input validation
  if (!is.matrix(edges) && !is.data.frame(edges)) {
    stop("edges must be a matrix or data.frame")
  }
  
  if (ncol(edges) != 2) {
    stop("edges must have exactly 2 columns")
  }
  
  # Convert to matrix
  if (is.data.frame(edges)) {
    edges <- as.matrix(edges)
  }
  
  # Keep as numeric to handle large integers
  edges <- matrix(as.numeric(edges), ncol = 2)
  
  if (any(is.na(edges))) {
    stop("edges contains NA values")
  }
  
  if (any(edges < 1, na.rm = TRUE)) {
    stop("All node IDs must be positive")
  }
  
  # Extract unique nodes and check memory requirements
  unique_nodes <- sort(unique(c(edges[, 1], edges[, 2])))
  n_unique_nodes <- length(unique_nodes)
  max_node_id <- max(unique_nodes)
  min_node_id <- min(unique_nodes)
  
  if (verbose) {
    cat("=== Memory Analysis ===\n")
    cat("Edges:", nrow(edges), "\n")
    cat("Unique nodes:", n_unique_nodes, "\n")
    cat("Node ID range:", min_node_id, "to", max_node_id, "\n")
    cat("Sparsity ratio:", round(n_unique_nodes / max_node_id * 100, 2), "%\n")
  }
  
  # Calculate memory requirements
  naive_memory_gb <- max_node_id * 12 / 1024^3  # Rough estimate for UnionFind
  efficient_memory_gb <- n_unique_nodes * 12 / 1024^3
  
  if (verbose) {
    cat("Naive memory (max ID):", round(naive_memory_gb, 2), "GB\n")
    cat("Efficient memory (unique):", round(efficient_memory_gb, 2), "GB\n")
  }
  
  # Always use remapping for safety and efficiency
  if (verbose) cat("Using node ID remapping for memory efficiency\n")
  
  # Create node mapping
  node_mapping <- data.frame(
    original = unique_nodes,
    mapped = 1:n_unique_nodes
  )

  # Remap edges to consecutive integers using fastmatch. fmatch() works
  # directly on the numeric node IDs, avoiding the slow, memory-heavy
  # as.character() + named-vector lookup that the previous version used
  # (that approach materialised a character key for every node and edge).
  edges_remapped <- matrix(0L, nrow = nrow(edges), ncol = 2)
  edges_remapped[, 1] <- fmatch(edges[, 1], unique_nodes)
  edges_remapped[, 2] <- fmatch(edges[, 2], unique_nodes)
  
  if (verbose) {
    cat("Calling C++ with", n_unique_nodes, "nodes instead of", max_node_id, "\n")
  }
  
  # Call C++ function with safe parameters
  result <- find_components_cpp(edges_remapped, n_unique_nodes, compress)
  
  # Map results back to original node IDs
  components_mapped <- setNames(result$components, node_mapping$original)
  
  if (verbose) {
    cat("Success! Found", result$n_components, "components\n")
    cat("Memory saved:", round(naive_memory_gb - efficient_memory_gb, 2), "GB\n")
  }
  
  return(list(
    components = components_mapped,
    component_sizes = result$component_sizes,
    n_components = result$n_components,
    node_mapping = node_mapping,
    memory_info = list(
      naive_memory_gb = naive_memory_gb,
      efficient_memory_gb = efficient_memory_gb,
      memory_saved_gb = naive_memory_gb - efficient_memory_gb
    )
  ))
}