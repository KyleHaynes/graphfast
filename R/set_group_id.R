
# Internal fast %in% alias (fastmatch); not exported, used below for filtering.
`%finn%` <- fastmatch::`%fin%`

#' Set Group ID Using Edge Components Approach
#'
#' Alternative implementation of group ID assignment using data.table melt and edge components.
#' This approach transforms the multi-column matching problem into a graph edge problem
#' by treating each row ID and each unique value as nodes, then finding connected components.
#'
#' @param dt A data.table containing the data to group
#' @param cols Character vector or regex pattern for columns to use for grouping. Default "phone".
#' @param var_output_name Character. Name of the output column to create. Default "gid".
#' @param incomparables Character vector of values to exclude from grouping (e.g., NA, "", "Unknown"). Default c(NA, "").
#'
#' @return The input data.table with a new group ID column added (modified by reference)
#'
#' @details
#' This function uses a graph-based approach:
#' 1. Melts the specified columns into long format (row_id -> value pairs)
#' 2. Creates a bipartite graph where row IDs and values are both nodes
#' 3. Finds connected components in this graph
#' 4. Maps the component IDs back to the original rows
#'
#' @examples
#' require("data.table")
#' dt <- data.table(
#'   id = 1:5,
#'   phone1 = c("123", "456", "123", "789", "456"),
#'   phone2 = c("111", "222", "333", "111", "222")
#' )
#' set_group_id(dt, cols = "phone", var_output_name = "group_id")
#' print(dt)
#'
#' @export
set_group_id <- function(dt, cols = "phone", var_output_name = "gid", incomparables = c(NA, "")) {
    
    # Input validation
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("data.table package is required for this function")
    }
    
    if (!data.table::is.data.table(dt)) {
        stop("dt must be a data.table")
    }
    
    if (!"id" %in% names(dt)) {
        stop("dt must have an 'id' column")
    }
    
    # Find columns matching the pattern
    matching_cols <- names(dt)[grepl(cols, names(dt))]
    if (length(matching_cols) == 0) {
        stop("No columns found matching pattern: ", cols)
    }
    
    # Step 1: Melt the data to create row_id -> value pairs
    # This transforms wide format (id, phone1, phone2) to long format (id, value)
    d <- data.table::melt(dt, "id", matching_cols)[, variable := NULL]
    
    # Step 2: Filter out incomparable values
    # Remove rows where the value should be excluded from grouping
    if (length(incomparables) > 0) {
        d <- d[!value %finn% incomparables]
    }
    
    # Skip processing if no valid values remain
    if (nrow(d) == 0) {
        dt[, (var_output_name) := 0L]
        return(dt[])
    }
    
    # Step 3: Create unique node identifiers for the bipartite graph
    # Add suffix to distinguish row IDs from values (in case of overlap)
    d[, id2 := paste0(id, "~")]
    
    # Step 4: Create unified node mapping
    # Get all unique nodes (both row IDs and values) and map to integers
    all_nodes <- unique(c(d$id2, d$value))
    
    # Step 5: Map nodes to integer IDs for efficient graph processing
    # Use fastmatch for optimal performance
    d[, value := fmatch(d[["value"]], all_nodes)]
    d[, id2 := fmatch(d[["id2"]], all_nodes)]
    
    # Step 6: Find connected components in the bipartite graph
    # Each edge connects a row ID to a value it contains
    # Connected components represent groups of rows that share values
    d[, group_id := edge_components(.SD, "value", "id2")]
    
    # Step 7: Map group IDs back to original data
    # Match original row IDs to get their group assignments
    dt[, (var_output_name) := d$group_id[fmatch(id, d$id)]]
    
    # Return the modified data.table
    return(dt[])
}

# Example usage and timing comparison
# system.time(set_group_id(dt, cols = "phone", var_output_name = "group_id"))
# system.time(group_id(dt, cols = "phone"))
