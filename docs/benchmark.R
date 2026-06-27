#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

has_igraph <- requireNamespace("igraph", quietly = TRUE)
has_network <- requireNamespace("network", quietly = TRUE)
has_sna <- requireNamespace("sna", quietly = TRUE)

if (!has_igraph) {
  stop("Please install the 'igraph' package to run this benchmark.")
}

load_graphfast <- function() {
  if (requireNamespace("graphfast", quietly = TRUE)) {
    suppressPackageStartupMessages(library(graphfast))
  } else if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".")
  } else {
    stop("Package 'graphfast' is not installed. Install it or install 'pkgload' so this script can load the package from source.")
  }

  if (!exists("find_components_cpp", envir = asNamespace("graphfast"), inherits = FALSE)) {
    if (requireNamespace("pkgload", quietly = TRUE)) {
      message("GraphFast C++ symbols not available in the installed package. Loading source package with pkgload...")
      pkgload::load_all(".")
    }
  }

  if (!exists("find_components_cpp", envir = asNamespace("graphfast"), inherits = FALSE)) {
    stop("GraphFast C++ symbols are not available. Please install graphfast with Rcpp support or run this script from the package source with pkgload installed.")
  }
}

load_graphfast()

cat("=== GraphFast benchmark against igraph/network/sna ===\n")
cat("Note: This script measures one timing pass per method and validates that outputs agree.\n")
cat("If network/sna are unavailable, those comparisons are skipped.\n\n")

sizes <- c(1e5, 1e6, 1e7)

make_graph <- function(n_rows) {
  n_rows <- as.integer(n_rows)
  n_nodes <- as.integer(max(1e5, min(1e6, floor(n_rows / 5))))
  set.seed(42)
  from <- sample(n_nodes, n_rows, replace = TRUE)
  to <- sample(n_nodes, n_rows, replace = TRUE)
  keep <- from != to
  if (sum(keep) < n_rows) {
    from <- from[keep]
    to <- to[keep]
    extra <- n_rows - length(from)
    if (extra > 0) {
      from <- c(from, sample(n_nodes, extra, replace = TRUE))
      to <- c(to, sample(n_nodes, extra, replace = TRUE))
      keep2 <- from != to
      if (sum(keep2) < n_rows) {
        stop("Could not generate enough non-self-loop edges")
      }
      from <- from[keep2][1:n_rows]
      to <- to[keep2][1:n_rows]
    }
  }
  edges <- matrix(as.integer(c(from[1:n_rows], to[1:n_rows])), ncol = 2)
  edges
}

make_queries <- function(edges, n_queries = 20L) {
  n_queries <- min(n_queries, nrow(edges))
  query_indices <- seq_len(n_queries)
  query_pairs <- edges[query_indices, , drop = FALSE]
  query_pairs
}

validate_membership <- function(base_membership, other_membership, node_pairs) {
  same <- TRUE
  for (i in seq_len(nrow(node_pairs))) {
    a <- node_pairs[i, 1]
    b <- node_pairs[i, 2]
    if (base_membership[a] == 0 || other_membership[a] == 0 ||
        base_membership[b] == 0 || other_membership[b] == 0) {
      next
    }
    if ((base_membership[a] == base_membership[b]) !=
        (other_membership[a] == other_membership[b])) {
      same <- FALSE
      break
    }
  }
  same
}

run_graphfast <- function(edges, query_pairs, n_nodes) {
  res <- list()
  res$components <- NULL
  res$component_time <- system.time({
    result <- find_connected_components(edges, n_nodes = n_nodes, compress = TRUE)
  })[3]
  res$components <- result$components
  res$n_components <- result$n_components
  res$shortest_time <- system.time({
    res$distances <- shortest_paths(edges, query_pairs, n_nodes = n_nodes)
  })[3]
  res$distances <- res$distances
  res
}

run_igraph <- function(edges, query_pairs, n_nodes) {
  res <- list()
  edges_char <- apply(edges, 2, as.character)
  vertex_names <- as.character(seq_len(n_nodes))
  edges_df <- data.frame(
    from = edges_char[, 1],
    to = edges_char[, 2],
    stringsAsFactors = FALSE
  )

  res$component_time <- system.time({
    g <- igraph::graph_from_data_frame(edges_df, directed = FALSE,
                                       vertices = data.frame(name = vertex_names, stringsAsFactors = FALSE))
    comp <- igraph::components(g)
  })[3]

  membership <- integer(n_nodes)
  names(membership) <- vertex_names
  membership[names(comp$membership)] <- comp$membership
  res$components <- membership
  res$n_components <- comp$no
  res$shortest_time <- system.time({
    res$distances <- vapply(seq_len(nrow(query_pairs)), function(i) {
      query <- as.character(query_pairs[i, ])
      d <- igraph::distances(g, v = query[1], to = query[2])
      d <- d[1, 1]
      if (is.infinite(d)) {
        -1L
      } else {
        as.integer(d)
      }
    }, integer(1L))
  })[3]
  res
}

run_network_sna <- function(edges, query_pairs, n_nodes) {
  res <- list()
  if (!has_network || !has_sna) {
    return(NULL)
  }
  res$component_time <- NA_real_
  res$shortest_time <- NA_real_
  res$components <- NULL
  res$distances <- NULL
  tryCatch({
    nw <- network::network(edges, directed = FALSE, matrix.type = "edgelist")
    component_fn <- if (exists("components", where = asNamespace("sna"), inherits = FALSE)) {
      sna::components
    } else if (exists("components", where = asNamespace("network"), inherits = FALSE)) {
      network::components
    } else {
      stop("No components() function available in sna or network")
    }
    res$component_time <- system.time({
      comp <- component_fn(nw)
    })[3]
    if (is.list(comp) && !is.null(comp$membership)) {
      res$components <- as.integer(comp$membership)
    } else if (is.numeric(comp)) {
      res$components <- as.integer(comp)
    } else {
      stop("Unexpected component object from network/sna")
    }
    res$n_components <- if (!is.null(comp$no)) as.integer(comp$no) else max(res$components)

    net_dist <- system.time({
      geod <- sna::geodist(nw, inf.replace = -1)
      if (!is.matrix(geod$gdist)) {
        stop("sna::geodist returned unexpected output")
      }
      res$distances <- vapply(seq_len(nrow(query_pairs)), function(i) {
        d <- geod$gdist[query_pairs[i, 1], query_pairs[i, 2]]
        if (is.infinite(d) || is.na(d)) {
          -1L
        } else {
          as.integer(d)
        }
      }, integer(1L))
    })[3]
    res$shortest_time <- net_dist
  }, error = function(e) {
    res$error <- conditionMessage(e)
  })
  res
}

print_header <- function(size) {
  cat(sprintf("\n=== Dataset: %s edges ===\n", format(size, scientific = FALSE, big.mark = ",")))
}

for (size in sizes) {
  print_header(size)
  edges <- make_graph(size)
  n_nodes <- max(edges)
  query_pairs <- make_queries(edges, n_queries = 20L)

  cat("Data generation complete. Nodes:", n_nodes, "Edges:", nrow(edges), "\n")

  gf <- run_graphfast(edges, query_pairs, n_nodes)
  cat(sprintf("graphfast: components=%.0f, comp_time=%.3fs, sp_time=%.3fs\n",
              gf$n_components, gf$component_time, gf$shortest_time))

  ig <- run_igraph(edges, query_pairs, n_nodes)
  cat(sprintf("igraph:    components=%.0f, comp_time=%.3fs, sp_time=%.3fs\n",
              ig$n_components, ig$component_time, ig$shortest_time))

  net <- NULL
  if (has_network && has_sna) {
    net <- run_network_sna(edges, query_pairs, n_nodes)
    if (!is.null(net$error)) {
      cat(sprintf("network/sna: skipped due to error: %s\n", net$error))
    } else {
      cat(sprintf("network/sna: components=%.0f, comp_time=%.3fs, sp_time=%.3fs\n",
                  net$n_components, net$component_time, net$shortest_time))
    }
  } else {
    cat("network/sna: skipped because one or both packages are unavailable\n")
  }

  cat("\nValidating connected-component results...\n")
  sample_pairs <- query_pairs
  if (has_network && !is.null(net) && is.null(net$error)) {
    net_membership <- net$components
    if (length(net_membership) != n_nodes) {
      net_membership <- rep(0L, n_nodes)
      warning("network/sna membership length differs from n_nodes; validation may be incomplete")
    }
    ok_net <- validate_membership(gf$components, net_membership, sample_pairs)
    cat("network/sna connected components match graphfast on sample pairs:", ok_net, "\n")
  }
  ok_igraph <- validate_membership(gf$components, ig$components, sample_pairs)
  cat("igraph connected components match graphfast on sample pairs:", ok_igraph, "\n")

  cat("\nValidating shortest-path results...\n")
  ig_match <- identical(gf$distances, ig$distances)
  cat("igraph shortest paths match graphfast:", ig_match, "\n")
  if (has_network && !is.null(net) && is.null(net$error)) {
    net_match <- identical(gf$distances, net$distances)
    cat("network/sna shortest paths match graphfast:", net_match, "\n")
  }
}

cat("\n=== Benchmark completed ===\n")
