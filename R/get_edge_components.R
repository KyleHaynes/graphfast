
#' Get Edge Component Assignments
#'
#' Efficiently returns component assignments for each edge in the input.
#' This is much faster than computing connected components separately and 
#' then doing lookups in R.
#'
#' @param edges A two-column matrix or data.frame where each row represents an edge
#' @param n_nodes Optional. Total number of nodes. If not provided, inferred from edges.
#' @param compress Logical. Whether to compress component IDs. Default is TRUE.
#' @param return_type Character. Either "list" (default) for separate from/to vectors,
#'   or "combined" for a single vector of from components only.
#'
#' @return If return_type="list": List with from_components, to_components, n_components.
#'   If return_type="combined": Integer vector of from_components (same length as input edges).
#'
#' @examples
#' edges <- matrix(c(1,2, 2,3, 5,6), ncol=2, byrow=TRUE)
#' get_edge_components(edges)
#' 
#' # For your specific use case:
#' get_edge_components(edges, return_type = "combined")
#'
#' @export
get_edge_components <- function(edges, n_nodes = NULL, compress = TRUE, return_type = "list") {
  # Input validation (same as find_connected_components)
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
         "Use get_edge_components_safe() for large integer support or ",
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
  
  # Determine number of nodes with memory safety check
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
  estimated_memory_gb <- n_nodes * 12 / 1024^3

  if (estimated_memory_gb > 8) {
    unique_nodes <- length(unique(c(edges[, 1], edges[, 2])))
    if (estimated_memory_gb > 32) {
      stop("Memory allocation would exceed 32GB (", round(estimated_memory_gb, 1),
           "GB) due to sparse large node IDs.\n",
           "Use get_edge_components_safe() instead.\n",
           "Your graph has ", unique_nodes, " unique nodes but max ID is ", n_nodes)
    }
    warning("Large memory allocation required (~", round(estimated_memory_gb, 1),
            "GB) due to sparse node IDs.\n",
            "Consider using get_edge_components_safe() which automatically ",
            "remaps node IDs.\n",
            "Unique nodes: ", unique_nodes, ", Max node ID: ", n_nodes)
  }
  
  # Call C++ function
  result <- get_edge_components_cpp(edges, n_nodes, compress)
  
  # Return based on requested type
  if (return_type == "combined") {
    return(result$from_components)
  } else if (return_type == "list") {
    return(result)
  } else {
    stop("return_type must be either 'list' or 'combined'")
  }
}
