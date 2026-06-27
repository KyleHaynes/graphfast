#' Multi-Column Group ID Assignment
#' 
#' High-performance grouping based on shared values across multiple columns.
#' Uses Union-Find with path compression for optimal performance.
#' Perfect for entity resolution, deduplication, and finding connected records.
#' 
#' @param data A data.frame or list of columns to group by
#' @param cols Character vector of column names or regex patterns to use for grouping (if data is data.frame)
#' @param use_regex Logical. Whether to treat 'cols' as regex patterns. Default TRUE.
#' @param incomparables Character vector of values to exclude from grouping (e.g., "", NA, "Unknown")
#' @param case_sensitive Logical. Whether string comparisons should be case sensitive. Default TRUE.
#' @param min_group_size Integer. Minimum group size to assign group ID (smaller groups get ID 0). Default 1.
#' @param return_details Logical. Whether to return detailed results including value mappings. Default FALSE.
#' @param verbose Logical. Whether to print timing and progress information. Default FALSE.
#' 
#' @return If return_details=FALSE: Integer vector of group IDs for each row
#'         If return_details=TRUE: List containing:
#'         \item{group_ids}{Integer vector of group IDs for each row}
#'         \item{n_groups}{Total number of groups found}
#'         \item{group_sizes}{Integer vector of group sizes}
#'         \item{value_map}{List showing which values belong to which groups}
#' 
#' @examples
#' # Basic usage with data.frame using exact column names
#' require(data.table)
#' df <- data.table(
#'   phone1 = c("123-456-7890", "987-654-3210", "123-456-7890", "", "555-0123"),
#'   phone2 = c("", "987-654-3210", "555-1234", "123-456-7890", ""),
#'   email = c("john@email.com", "jane@email.com", "bob@email.com", "john@email.com", "alice@email.com"),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Group by phone columns using regex (default)
#' group_ids <- group_id(df, cols = "phone", incomparables = c(""))
#' print(group_ids)
#' 
#' # Within a data.table
#' # Group by phone columns using regex (default)
#' dt2 <- copy(df)
#' dt2[, gid := group_id(dt2, cols = "phone", incomparables = c(""))]
#' print(dt2)
#' 
#' # Group by phone columns using exact names
#' group_ids <- group_id(df, cols = c("phone1", "phone2"), use_regex = FALSE, incomparables = c(""))
#' print(group_ids)
#' 
#' # Group by all columns matching pattern with detailed output
#' result <- group_id(df, cols = "phone|email", 
#'                    incomparables = c("", "NA"), 
#'                    return_details = TRUE)
#' print(result)
#' 
#' # Direct list input
#' phone1 <- c("123-456-7890", "987-654-3210", "123-456-7890", "", "555-0123")
#' phone2 <- c("", "987-654-3210", "555-1234", "123-456-7890", "")
#' email <- c("john@email.com", "jane@email.com", "bob@email.com", "john@email.com", "alice@email.com")
#' 
#' group_id(list(phone1, phone2, email), incomparables = c(""))
#' 
#' @export
group_id <- function(data, 
                     cols = NULL,
                     use_regex = TRUE,
                     incomparables = c("", "NA", "Unknown"),
                     case_sensitive = TRUE,
                     min_group_size = 1,
                     return_details = FALSE,
                     verbose = FALSE) {
  
  # Input validation
  if (is.null(data) || length(data) == 0) {
    stop("data cannot be NULL or empty")
  }
  
  # Handle data.frame input
  if (is.data.frame(data)) {
    if (is.null(cols)) {
      # Use all columns if none specified
      cols <- names(data)
    } else if (use_regex) {
      # Expand regex patterns to matching column names
      all_cols <- names(data)
      expanded_cols <- character(0)
      
      for (pattern in cols) {
        matches <- grep(pattern, all_cols, value = TRUE, perl = TRUE)
        expanded_cols <- c(expanded_cols, matches)
      }
      
      # Remove duplicates and preserve order
      expanded_cols <- unique(expanded_cols)
      
      if (length(expanded_cols) == 0) {
        stop("No columns match the specified patterns: ", paste(cols, collapse = ", "))
      }
      
      # Update cols to be the expanded column names
      cols <- expanded_cols
    }
    
    # Validate column names (exact matches when not using regex, or after regex expansion)
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
    }
    
    # Extract specified columns as list - use proper data.table syntax with optimization
    if (is.data.table(data)) {
      # Optimize: Use direct column extraction without intermediate variables
      data_list <- as.list(data[, ..cols])
    } else {
      data_list <- data[cols]
    }
  } else if (is.list(data)) {
    # Use list directly
    data_list <- data
    
    if (!is.null(cols)) {
      warning("cols parameter ignored when data is already a list")
    }
  } else {
    stop("data must be a data.frame or list")
  }
  
  # Validate incomparables - optimize string conversion
  if (!is.character(incomparables)) {
    # Fast conversion avoiding repeated allocations
    incomparables <- as.character(incomparables)
  }
  
  # Pre-filter empty incomparables for C++ efficiency
  incomparables <- incomparables[nzchar(incomparables) & !is.na(incomparables)]
  
  if (verbose) {
    n_rows <- if(is.list(data_list)) length(data_list[[1]]) else nrow(data_list)
    cat("Processing", n_rows, "rows across", length(data_list), "columns\n")
    start_time <- Sys.time()
  }
  
  # Check if we can use the fast numeric-only path
  all_numeric <- all(sapply(data_list, function(x) is.numeric(x) || is.integer(x)))
  has_incomparables <- length(incomparables) > 0
  
  if (all_numeric && !has_incomparables && case_sensitive) {
    # Use ultra-fast numeric-only C++ function (optimized for millions of records)
    if (verbose) cat("Using ultra-fast numeric algorithm for large datasets\n")
    result <- ultra_fast_group_numeric_cpp(
      data = data_list,
      min_group_size = min_group_size
    )
  } else {
    # Use general string-based C++ function
    if (verbose) cat("Using general string-based algorithm\n")
    result <- multi_column_group_cpp(
      data = data_list,
      incomparables = incomparables,
      case_sensitive = case_sensitive,
      min_group_size = min_group_size
    )
  }
  
  if (verbose) {
    total_time <- Sys.time() - start_time
    cat("C++ processing completed in", format(total_time, digits = 3), "\n")
    cat("Found", result$n_groups, "groups from", length(result$group_ids), "records\n")
  }
  
  # Validate result
  if (is.null(result) || !is.list(result) || is.null(result$group_ids)) {
    stop("C++ function returned invalid result")
  }
  
  # Return based on return_details flag
  if (return_details) {
    # Add some additional metadata
    result$call <- match.call()
    result$settings <- list(
      case_sensitive = case_sensitive,
      min_group_size = min_group_size,
      incomparables = incomparables,
      n_columns = length(data_list)
    )
    
    # Add informative class
    class(result) <- c("group_id_result", "list")
    
    return(result)
  } else {
    # Return just the group IDs
    return(result$group_ids)
  }
}

#' Print method for group_id_result objects
#' @param x A group_id_result object
#' @param ... Additional arguments (ignored)
#' @export
print.group_id_result <- function(x, ...) {
  cat("Multi-Column Group ID Results\n")
  cat("=============================\n")
  cat("Total rows:", length(x$group_ids), "\n")
  cat("Number of groups:", x$n_groups, "\n")
  cat("Group sizes:", paste(x$group_sizes, collapse = ", "), "\n")
  cat("Columns used:", x$settings$n_columns, "\n")
  cat("Case sensitive:", x$settings$case_sensitive, "\n")
  cat("Min group size:", x$settings$min_group_size, "\n")
  cat("Incomparables:", paste(x$settings$incomparables, collapse = ", "), "\n")
  cat("\nGroup IDs (first 20):\n")
  print(head(x$group_ids, 20))
  
  if (length(x$value_map) > 0) {
    cat("\nShared values creating groups (first 10):\n")
    for (i in seq_len(min(10, length(x$value_map)))) {
      cat(sprintf("'%s': rows %s\n", 
                  names(x$value_map)[i], 
                  paste(x$value_map[[i]], collapse = ", ")))
    }
    
    if (length(x$value_map) > 10) {
      cat("... and", length(x$value_map) - 10, "more values\n")
    }
  }
}

#' Data.table integration for group_id
#' 
#' Adds group IDs as a new column to a data.table
#' 
#' @param dt A data.table
#' @param cols Character vector of column names or regex patterns to use for grouping
#' @param group_col Name of the new column to create (default: "group_id")
#' @param use_regex Logical. Whether to treat 'cols' as regex patterns. Default TRUE.
#' @param ... Additional arguments passed to group_id()
#' 
#' @return The data.table with the new group_id column added
#' 
#' @examples
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   library(data.table)
#'   
#'   dt <- data.table(
#'     phone1 = c("123-456-7890", "987-654-3210", "123-456-7890", "", "555-0123"),
#'     phone2 = c("", "987-654-3210", "555-1234", "123-456-7890", ""),
#'     email = c("john@email.com", "jane@email.com", "bob@email.com", "john@email.com", "alice@email.com")
#'   )
#'   
#'   # Add group IDs based on phone columns using regex
#'   add_group_ids(dt, cols = "phone", incomparables = c(""))
#'   print(dt)
#'   
#'   # Add group IDs based on exact column names
#'   add_group_ids(dt, cols = c("phone1", "phone2"), use_regex = FALSE, incomparables = c(""))
#'   print(dt)
#' }
#' 
#' @export
add_group_ids <- function(dt, cols, group_col = "group_id", use_regex = TRUE, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package is required for this function")
  }
  
  if (!data.table::is.data.table(dt)) {
    stop("dt must be a data.table")
  }
  
  # Calculate group IDs
  group_ids <- group_id(dt, cols = cols, use_regex = use_regex, return_details = FALSE, ...)
  
  # Add to data.table by reference
  data.table::set(dt, j = group_col, value = group_ids)
  
  return(invisible(dt[]))
}