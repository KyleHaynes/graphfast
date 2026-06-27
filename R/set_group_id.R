
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
#'   Ignored when \code{isolate} is supplied.
#' @param var_output_name Character. Name of the output column to create. Default "gid".
#' @param incomparables Character vector of values to exclude from grouping (e.g., NA, "", "Unknown"). Default c(NA, "").
#' @param isolate Optional list of character vectors of column names. Each element defines a set of
#'   columns whose values share a namespace. Identical values only link rows when they occur within the
#'   same set, so e.g. a phone number sitting in an email column will not connect to phone columns.
#'   The columns to group on are taken from the union of all sets; \code{cols} is ignored when supplied.
#'   See examples. Default NULL (all matching columns share one namespace, the original behaviour).
#' @param return_edges Logical. If TRUE, return the record-to-record edge list of all pair connections
#'   (every pair of \code{id}s that share a value) instead of the data.table. The group-id column is
#'   still added to \code{dt} by reference. Useful for plotting the linkage with e.g. \pkg{visNetwork}.
#'   Default FALSE.
#'
#' @return By default the input data.table with a new group ID column added (modified by reference).
#'   If \code{return_edges = TRUE}, a data.table edge list with columns \code{from}, \code{to}
#'   (the connected \code{id}s, with \code{from < to}) and \code{value} (the shared value linking
#'   them); the group-id column is still written to \code{dt} by reference.
#'
#' @details
#' This function uses a graph-based approach:
#' 1. Melts the specified columns into long format (row_id -> value pairs)
#' 2. Creates a bipartite graph where row IDs and values are both nodes
#' 3. Finds connected components in this graph
#' 4. Maps the component IDs back to the original rows
#'
#' When \code{isolate} is supplied, each value is prefixed with the index of the column set it came
#' from before the graph is built, so values from different sets never collide even when they are
#' textually identical.
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
#' # Treat phone columns and email in isolation: a phone number appearing in the
#' # email column will not link to the phone columns.
#' dt2 <- data.table(
#'   id = 1:5,
#'   phone1 = c("123-456-7890", "987-654-3210", "123-456-7890", "", "555-0123"),
#'   phone2 = c("", "987-654-3210", "555-1234", "123-456-7890", ""),
#'   email = c("john@email.com", "jane@email.com", "bob@email.com",
#'             "john@email.com", "123-456-7890")
#' )
#' set_group_id(dt2, isolate = list(c("phone1", "phone2"), "email"))
#' print(dt2)
#'
#' # Return the edge list of all pair connections (e.g. to plot with visNetwork).
#' edges <- set_group_id(dt2, isolate = list(c("phone1", "phone2"), "email"),
#'                       return_edges = TRUE)
#' edges
#' \dontrun{
#' # nodes carry the group id assigned above; edges carry the shared value
#' nodes <- data.frame(id = dt2$id, label = dt2$id, group = dt2$gid)
#' visNetwork::visNetwork(nodes, edges)
#' }
#'
#' @export
set_group_id <- function(dt, cols = "phone", var_output_name = "gid", incomparables = c(NA, ""),
                         isolate = NULL, return_edges = FALSE) {

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

    if (is.null(isolate)) {
        # Find columns matching the pattern
        matching_cols <- names(dt)[grepl(cols, names(dt))]
        if (length(matching_cols) == 0) {
            stop("No columns found matching pattern: ", cols)
        }
        # No isolation: every column shares a single namespace.
        set_of_col <- NULL
    } else {
        # Each element of `isolate` is a set of columns sharing a value namespace.
        if (!is.list(isolate)) {
            stop("isolate must be a list of character vectors of column names")
        }
        # Map each column to the index of the set it belongs to. If a column is
        # listed in more than one set, the last occurrence wins.
        set_of_col <- stats::setNames(
            rep.int(seq_along(isolate), lengths(isolate)),
            unlist(isolate, use.names = FALSE)
        )
        matching_cols <- names(set_of_col)
        missing_cols <- setdiff(matching_cols, names(dt))
        if (length(missing_cols) > 0) {
            stop("Columns in `isolate` not found in dt: ", paste(missing_cols, collapse = ", "))
        }
    }

    # Step 1: Melt the data to create row_id -> value pairs
    # This transforms wide format (id, phone1, phone2) to long format (id, value).
    # `variable` (the source column) is kept here so values can be namespaced per
    # isolation set; it is dropped immediately when no isolation is requested.
    d <- data.table::melt(dt, "id", matching_cols)
    if (is.null(set_of_col)) {
        d[, variable := NULL]
    }

    # Step 2: Filter out incomparable values
    # Remove rows where the value should be excluded from grouping
    if (length(incomparables) > 0) {
        d <- d[!value %finn% incomparables]
    }

    # Step 2b: Namespace values by their isolation set so that textually identical
    # values from different sets (e.g. a phone number in an email column) do not
    # connect rows. "\u0001" (SOH) is used as a separator that will not occur in data.
    if (!is.null(set_of_col) && nrow(d) > 0) {
        d[, value := paste0(set_of_col[as.character(variable)], "\u0001", value)]
        d[, variable := NULL]
    }

    # Skip processing if no valid values remain
    if (nrow(d) == 0) {
        dt[, (var_output_name) := 0L]
        if (return_edges) {
            return(data.table::data.table(from = dt$id[0L], to = dt$id[0L],
                                          value = character(0)))
        }
        return(dt[])
    }

    # Optional: build the record-to-record edge list of all pair connections.
    # Two records are connected when they share a value, so self-join the long
    # table on the (namespaced) value and keep each unordered pair once.
    if (return_edges) {
        long <- d[, .(id, value)]
        edge_list <- merge(long, long, by = "value", allow.cartesian = TRUE)
        edge_list <- edge_list[id.x < id.y]
        # Strip the isolation-set prefix so the reported value is human-readable.
        if (!is.null(set_of_col)) {
            edge_list[, value := sub("^[0-9]+\u0001", "", value)]
        }
        edge_list <- unique(edge_list[, .(from = id.x, to = id.y, value)])
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
    
    # Return the edge list of pair connections when requested, otherwise the
    # modified data.table (the group-id column is set by reference either way).
    if (return_edges) {
        return(edge_list[])
    }
    return(dt[])
}

# Example usage and timing comparison
# system.time(set_group_id(dt, cols = "phone", var_output_name = "group_id"))
# system.time(group_id(dt, cols = "phone"))
