#' Find Connected Components in Large Graphs
#'
#' Efficiently finds all connected components in a graph represented by edge pairs.
#' Uses optimized C++ algorithms with Union-Find data structure for handling
#' hundreds of millions of edges.
#'
#' @param edges A two-column matrix or data.frame where each row represents an edge
#'   between two nodes. Nodes should be represented as integers starting from 1.
#' @param n_nodes Optional. Total number of nodes in the graph. If not provided,
#'   will be inferred from the maximum node ID in edges.
#' @param compress Logical. Whether to compress node IDs to consecutive integers.
#'   Useful when node IDs are sparse. Default is TRUE.
#'
#' @return A list containing:
#' \item{components}{Integer vector where each element represents the component ID
#'   for the corresponding node}
#' \item{component_sizes}{Integer vector of component sizes}
#' \item{n_components}{Total number of connected components}
#'
#' @examples
#' # Create a simple graph with 3 components
#' edges <- matrix(c(1,2, 2,3, 5,6, 8,9, 9,10), ncol=2, byrow=TRUE)
#' result <- find_connected_components(edges)
#' print(result$n_components)  # Should be 3
#'
#' @export
find_connected_components <- function(edges, n_nodes = NULL, compress = TRUE) {
  # Input validation
  if (!is.matrix(edges) && !is.data.frame(edges)) {
    stop("edges must be a matrix or data.frame")
  }
  
  if (ncol(edges) != 2) {
    stop("edges must have exactly 2 columns")
  }
  
  # Convert to matrix if data.frame
  if (is.data.frame(edges)) {
    edges <- as.matrix(edges)
  }
  
  # Check for large integers before conversion
  edges_numeric <- matrix(as.numeric(edges), ncol = 2)
  max_safe_int <- .Machine$integer.max  # 2,147,483,647
  
  if (any(edges_numeric > max_safe_int, na.rm = TRUE)) {
    large_vals <- unique(edges_numeric[edges_numeric > max_safe_int & !is.na(edges_numeric)])
    stop("Node IDs exceed 32-bit integer limit (", max_safe_int, "). ",
         "Large values found: ", paste(head(large_vals, 3), collapse = ", "), 
         if(length(large_vals) > 3) "..." else "", ". ",
         "Use find_connected_components_large() for large integer support or ",
         "remap your node IDs to smaller consecutive integers.")
  }
  
  # Convert to integer (safe now)
  edges <- matrix(as.integer(edges_numeric), ncol = 2)
  
  # Check for invalid values
  if (any(edges < 1, na.rm = TRUE)) {
    stop("All node IDs must be positive integers >= 1")
  }
  
  if (any(is.na(edges))) {
    stop("edges contains NA values. This may indicate integer overflow from large node IDs.")
  }
  
  # Determine number of nodes
  if (is.null(n_nodes)) {
    n_nodes <- max(edges)
  } else {
    n_nodes <- as.integer(n_nodes)
    if (n_nodes < max(edges)) {
      stop("n_nodes must be at least as large as the maximum node ID in edges")
    }
  }
  
  # Memory safety check. The estimate depends only on the max node ID, so the
  # expensive unique() scan is deferred until we actually need it for a message.
  estimated_memory_gb <- n_nodes * 12 / 1024^3  # Rough estimate

  if (estimated_memory_gb > 8) {  # Warning for >8GB allocation
    unique_nodes <- length(unique(c(edges[, 1], edges[, 2])))
    if (estimated_memory_gb > 32) {  # Hard stop for >32GB
      stop("Memory allocation would exceed 32GB (", round(estimated_memory_gb, 1),
           "GB) due to sparse large node IDs.\n",
           "Use find_connected_components_safe() instead, which handles large sparse node IDs efficiently.\n",
           "Your graph has ", unique_nodes, " unique nodes but max ID is ", n_nodes)
    }
    warning("Large memory allocation required (~", round(estimated_memory_gb, 1),
            "GB) due to sparse node IDs.\n",
            "Consider using find_connected_components_safe() which automatically ",
            "remaps node IDs.\n",
            "Unique nodes: ", unique_nodes, ", Max node ID: ", n_nodes)
  }
  
  # Call C++ function
  result <- find_components_cpp(edges, n_nodes, compress)
  
  return(result)
}
