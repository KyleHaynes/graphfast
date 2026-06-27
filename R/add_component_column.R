
#' Add Component Column to Data.Table
#'
#' Efficiently adds a component ID column directly to an existing data.table.
#' You can specify which columns represent the edges (from/to nodes).
#' Since edges connect nodes in the same component, only one component ID per edge is needed.
#' 
#' This function uses fastmatch::fmatch() for optimal node mapping performance on large datasets.
#'
#' @param dt A data.table containing edge information
#' @param from_col Character. Name of the column containing 'from' node IDs. Default "from".
#' @param to_col Character. Name of the column containing 'to' node IDs. Default "to".
#' @param component_col Character. Name for the new component column. Default "component".
#' @param n_nodes Optional. Total number of nodes. If not provided, inferred from data.
#' @param compress Logical. Whether to compress component IDs. Default is TRUE.
#' @param in_place Logical. Whether to modify the data.table in place (TRUE) or return a copy (FALSE). Default TRUE.
#' @param verbose Logical. Whether to print timing information. Default is FALSE.
#'
#' @return If in_place=TRUE, modifies dt and returns it invisibly. If in_place=FALSE, returns a copy of dt with the new column.
#'
#' @examples
#' # Create a data.table with edges
#' require("data.table")
#' dt <- data.table(source = c(1,2,5), target = c(2,3,6), weight = c(0.5, 0.8, 0.3))
#' dt
#' 
#' # Add component column (modifies dt in place)
#' add_component_column(dt, from_col = "source", to_col = "target")
#' # Now dt has a 'component' column
#' 
#' # Or specify custom column name and don't modify original
#' dt2 <- add_component_column(dt, from_col = "source", to_col = "target",
#'                             component_col = "group_id", in_place = FALSE)
#' dt2
#' 
#' # For large datasets, use verbose=TRUE to monitor performance
#' \dontrun{
#' large_dt <- data.table(from = sample(1:1e6, 1e7, replace = TRUE),
#'                        to = sample(1:1e6, 1e7, replace = TRUE))
#' add_component_column(large_dt, verbose = TRUE)
#' # Shows timing for each step and number of components found
#' }
#' 
#' @export
add_component_column <- function(dt, from_col = "from", to_col = "to", 
                                component_col = "component", n_nodes = NULL, 
                                compress = TRUE, in_place = TRUE, verbose = FALSE) {
  
  # Input validation
  if (!is.data.table(dt)) {
    stop("dt must be a data.table")
  }
  
  if (!from_col %in% names(dt)) {
    stop("Column '", from_col, "' not found in data.table")
  }
  
  if (!to_col %in% names(dt)) {
    stop("Column '", to_col, "' not found in data.table")
  }
  
  if (component_col %in% names(dt)) {
    warning("Column '", component_col, "' already exists and will be overwritten")
  }
  
  if (verbose) {
    cat("Processing", nrow(dt), "edges with", length(unique(c(dt[[from_col]], dt[[to_col]]))), "unique nodes\n")
    start_time <- Sys.time()
  }
  
  # Extract edge matrix with optimized node mapping using fastmatch
  if (verbose) cat("Extracting and mapping edge matrix (using fastmatch)...")
  matrix_start <- if(verbose) Sys.time() else NULL
  
  # Fast unified node mapping: get all unique nodes once, then map both columns
  all_nodes <- unique(c(dt[[from_col]], dt[[to_col]]))
  
  # Create optimized edge matrix with smallest possible integers using fmatch (faster than match)
  edges_matrix <- matrix(c(
    fmatch(dt[[from_col]], all_nodes),
    fmatch(dt[[to_col]], all_nodes)
  ), ncol = 2)
  

  if (verbose) {
    matrix_time <- Sys.time() - matrix_start
    cat(" completed in", format(matrix_time, digits = 3), "\n")
    cat("Mapped", length(all_nodes), "unique nodes to integers 1-", length(all_nodes), "\n")
    cat("Computing connected components...")
  }
  
  # Get component IDs for each edge
  components_start <- if(verbose) Sys.time() else NULL
  component_ids <- group_edges(edges_matrix, n_nodes = n_nodes, compress = compress)
  
  if (verbose) {
    components_time <- Sys.time() - components_start
    cat(" completed in", format(components_time, digits = 3), "\n")
    cat("Adding component column...")
  }
  
  # Add to data.table
  assign_start <- if(verbose) Sys.time() else NULL
  
  if (in_place) {
    # Modify in place
    dt[, (component_col) := component_ids]
  } else {
    # Return a copy
    dt_copy <- copy(dt)
    dt_copy[, (component_col) := component_ids]
  }
  
  if (verbose) {
    assign_time <- Sys.time() - assign_start
    total_time <- Sys.time() - start_time
    cat(" completed in", format(assign_time, digits = 3), "\n")
    cat("Total time:", format(total_time, digits = 3), "\n")
    cat("Found", length(unique(component_ids)), "connected components\n")
  }
  
  if(in_place){
    return(dt[])
  } else {
    return(dt_copy[])
  }
}
