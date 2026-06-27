#include <Rcpp.h>
#include <vector>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>

// Forward declarations to avoid conflicts
class UnionFind {
private:
    std::vector<int> parent;
    std::vector<int> rank;
    
public:
    UnionFind(int n) : parent(n), rank(n, 0) {
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }
    
    // Iterative find with path halving. Avoids deep recursion (and stack
    // overflow) on very large graphs while keeping near-constant amortised cost.
    int find(int x) {
        while (parent[x] != x) {
            parent[x] = parent[parent[x]];  // path halving
            x = parent[x];
        }
        return x;
    }
    
    bool union_sets(int x, int y) {
        int px = find(x);
        int py = find(y);
        
        if (px == py) return false;
        
        if (rank[px] < rank[py]) {
            parent[px] = py;
        } else if (rank[px] > rank[py]) {
            parent[py] = px;
        } else {
            parent[py] = px;
            rank[px]++;
        }
        return true;
    }
    
    bool connected(int x, int y) {
        return find(x) == find(y);
    }
};

//' Find Connected Components
// [[Rcpp::export]]
Rcpp::List find_components_cpp(const Rcpp::IntegerMatrix& edges, int n_nodes, bool compress = true) {
    UnionFind uf(n_nodes);
    
    for (int i = 0; i < edges.nrow(); i++) {
        int u = edges(i, 0) - 1;
        int v = edges(i, 1) - 1;
        
        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes) {
            uf.union_sets(u, v);
        }
    }
    
    // Relabel roots to compact component IDs using an O(n) vector lookup
    // (roots live in [0, n_nodes), so std::map is unnecessary overhead).
    std::vector<int> root_to_comp(n_nodes, -1);
    std::vector<int> roots(n_nodes);
    int next_component_id = 0;

    for (int i = 0; i < n_nodes; i++) {
        int root = uf.find(i);
        roots[i] = root;
        if (root_to_comp[root] == -1) {
            root_to_comp[root] = next_component_id++;
        }
    }

    // Component sizes are always populated (previously empty when compress=FALSE),
    // and labels are 1-based consecutive when compressing, raw roots otherwise.
    std::vector<int> components(n_nodes);
    std::vector<int> component_sizes(next_component_id, 0);
    for (int i = 0; i < n_nodes; i++) {
        int comp = root_to_comp[roots[i]];
        component_sizes[comp]++;
        components[i] = compress ? comp + 1 : roots[i];
    }

    return Rcpp::List::create(
        Rcpp::Named("components") = components,
        Rcpp::Named("component_sizes") = component_sizes,
        Rcpp::Named("n_components") = next_component_id
    );
}

//' Check Connectivity
// [[Rcpp::export]]
Rcpp::LogicalVector are_connected_cpp(const Rcpp::IntegerMatrix& edges, const Rcpp::IntegerMatrix& query_pairs, int n_nodes) {
    UnionFind uf(n_nodes);
    
    for (int i = 0; i < edges.nrow(); i++) {
        int u = edges(i, 0) - 1;
        int v = edges(i, 1) - 1;
        
        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes) {
            uf.union_sets(u, v);
        }
    }
    
    Rcpp::LogicalVector result(query_pairs.nrow());
    for (int i = 0; i < query_pairs.nrow(); i++) {
        int u = query_pairs(i, 0) - 1;
        int v = query_pairs(i, 1) - 1;
        
        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes) {
            result[i] = uf.connected(u, v);
        } else {
            result[i] = false;
        }
    }
    
    return result;
}

//' Shortest Paths
// [[Rcpp::export]]
Rcpp::IntegerVector shortest_paths_cpp(const Rcpp::IntegerMatrix& edges, const Rcpp::IntegerMatrix& query_pairs, 
                                      int n_nodes, int max_distance) {
    
    // Build the adjacency list, reserving exact degree per node so we avoid
    // repeated vector reallocations while filling (a real cost on large graphs).
    std::vector<std::vector<int>> adj(n_nodes);
    {
        std::vector<int> degree(n_nodes, 0);
        for (int i = 0; i < edges.nrow(); i++) {
            int u = edges(i, 0) - 1;
            int v = edges(i, 1) - 1;
            if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes && u != v) {
                degree[u]++;
                degree[v]++;
            }
        }
        for (int i = 0; i < n_nodes; i++) adj[i].reserve(degree[i]);
    }

    for (int i = 0; i < edges.nrow(); i++) {
        int u = edges(i, 0) - 1;
        int v = edges(i, 1) - 1;

        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes && u != v) {
            adj[u].push_back(v);
            adj[v].push_back(u);
        }
    }

    Rcpp::IntegerVector result(query_pairs.nrow());

    // Reuse a single distance buffer across all queries. A monotonically
    // increasing "version" marks visited nodes, so we never re-zero an
    // O(n_nodes) array per query (the previous behaviour, which dominated
    // runtime when answering many queries on a large graph).
    std::vector<int> visited_version(n_nodes, 0);
    std::vector<int> distance(n_nodes, 0);
    int version = 0;
    std::queue<int> bfs_queue;

    for (int q = 0; q < query_pairs.nrow(); q++) {
        int source = query_pairs(q, 0) - 1;
        int target = query_pairs(q, 1) - 1;

        if (source < 0 || source >= n_nodes || target < 0 || target >= n_nodes) {
            result[q] = -1;
            continue;
        }

        if (source == target) {
            result[q] = 0;
            continue;
        }

        version++;
        while (!bfs_queue.empty()) bfs_queue.pop();  // clear any leftover state
        distance[source] = 0;
        visited_version[source] = version;
        bfs_queue.push(source);

        bool found = false;
        while (!bfs_queue.empty() && !found) {
            int current = bfs_queue.front();
            bfs_queue.pop();

            if (max_distance > 0 && distance[current] >= max_distance) {
                break;
            }

            for (int neighbor : adj[current]) {
                if (visited_version[neighbor] != version) {
                    visited_version[neighbor] = version;
                    distance[neighbor] = distance[current] + 1;

                    if (neighbor == target) {
                        result[q] = distance[neighbor];
                        found = true;
                        break;
                    }

                    bfs_queue.push(neighbor);
                }
            }
        }

        if (!found) {
            result[q] = -1;
        }
    }

    return result;
}

//' Graph Statistics
// [[Rcpp::export]]
Rcpp::List graph_stats_cpp(const Rcpp::IntegerMatrix& edges, int n_nodes) {
    std::vector<int> degree(n_nodes, 0);
    int n_edges = edges.nrow();
    
    for (int i = 0; i < n_edges; i++) {
        int u = edges(i, 0) - 1;
        int v = edges(i, 1) - 1;
        
        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes && u != v) {
            degree[u]++;
            degree[v]++;
        }
    }
    
    int min_degree = *std::min_element(degree.begin(), degree.end());
    int max_degree = *std::max_element(degree.begin(), degree.end());
    double mean_degree = 0.0;
    for (int d : degree) {
        mean_degree += d;
    }
    mean_degree /= n_nodes;
    
    double max_possible_edges = (double)n_nodes * (n_nodes - 1) / 2.0;
    double density = (max_possible_edges > 0) ? n_edges / max_possible_edges : 0.0;
    
    Rcpp::List degree_stats = Rcpp::List::create(
        Rcpp::Named("min") = min_degree,
        Rcpp::Named("max") = max_degree,
        Rcpp::Named("mean") = mean_degree
    );
    
    return Rcpp::List::create(
        Rcpp::Named("n_edges") = n_edges,
        Rcpp::Named("n_nodes") = n_nodes,
        Rcpp::Named("density") = density,
        Rcpp::Named("degree_stats") = degree_stats
    );
}

//' Get Edge Component Assignments
//' 
//' Efficiently returns component ID for each edge (both from and to nodes).
//' This is much faster than doing component lookup in R.
//' 
//' @param edges IntegerMatrix with two columns (from, to)
//' @param n_nodes Number of nodes in the graph
//' @param compress Whether to compress component IDs to consecutive integers
//' @return List with from_components and to_components vectors
// [[Rcpp::export]]
Rcpp::List get_edge_components_cpp(const Rcpp::IntegerMatrix& edges, int n_nodes, bool compress = true) {
    UnionFind uf(n_nodes);
    
    // Build the union-find structure
    for (int i = 0; i < edges.nrow(); i++) {
        int u = edges(i, 0) - 1;  // Convert to 0-based indexing
        int v = edges(i, 1) - 1;
        
        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes) {
            uf.union_sets(u, v);
        }
    }
    
    // Create component mapping (same logic as find_components_cpp) using an
    // O(n) vector lookup instead of std::map.
    std::vector<int> root_to_comp(n_nodes, -1);
    std::vector<int> node_components(n_nodes);
    int next_component_id = 0;

    for (int i = 0; i < n_nodes; i++) {
        int root = uf.find(i);
        if (root_to_comp[root] == -1) {
            root_to_comp[root] = next_component_id++;
        }
        // 1-based consecutive IDs when compressing, raw roots otherwise.
        node_components[i] = compress ? root_to_comp[root] + 1 : root;
    }

    // Now assign components directly to edges
    std::vector<int> from_components(edges.nrow());
    std::vector<int> to_components(edges.nrow());
    
    for (int i = 0; i < edges.nrow(); i++) {
        int u = edges(i, 0) - 1;  // Convert to 0-based
        int v = edges(i, 1) - 1;
        
        if (u >= 0 && u < n_nodes && v >= 0 && v < n_nodes) {
            from_components[i] = node_components[u];
            to_components[i] = node_components[v];
        } else {
            // Invalid node - assign -1 or 0
            from_components[i] = compress ? 0 : -1;
            to_components[i] = compress ? 0 : -1;
        }
    }
    
    return Rcpp::List::create(
        Rcpp::Named("from_components") = from_components,
        Rcpp::Named("to_components") = to_components,
        Rcpp::Named("n_components") = next_component_id
    );
}

//' Multi-Column Group ID Assignment
//'
//' High-performance grouping based on shared values across multiple columns.
//' Uses Union-Find with path compression for optimal performance.
//' Perfect for entity resolution, deduplication, and finding connected records.
//'
//' @param data List of character/numeric vectors representing columns to group by
//' @param incomparables Character vector of values to exclude from grouping (e.g., "", NA, "Unknown")
//' @param case_sensitive Logical. Whether string comparisons should be case sensitive. Default TRUE.
//' @param min_group_size Integer. Minimum group size to assign group ID (smaller groups get ID 0). Default 1.
//'
//' @return List containing:
//' \item{group_ids}{Integer vector of group IDs for each row}
//' \item{n_groups}{Total number of groups found}
//' \item{group_sizes}{Integer vector of group sizes}
//' \item{value_map}{List showing which values belong to which groups}
//'
//' @examples
//' # Phone number matching across columns
//' phone1 <- c("123-456-7890", "987-654-3210", "123-456-7890", "", "555-0123")
//' phone2 <- c("", "987-654-3210", "555-1234", "123-456-7890", "")
//' email <- c("john@email.com", "jane@email.com", "bob@email.com", "john@email.com", "alice@email.com")
//' 
//' result <- multi_column_group_cpp(list(phone1, phone2, email), 
//'                                  incomparables = c("", "NA", "Unknown"))
//'
// [[Rcpp::export]]
Rcpp::List multi_column_group_cpp(const Rcpp::List& data,
                                  const Rcpp::CharacterVector& incomparables = Rcpp::CharacterVector::create(),
                                  bool case_sensitive = true,
                                  int min_group_size = 1) {
    
    int n_rows = 0;
    int n_cols = data.size();
    
    if (n_cols == 0) {
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = Rcpp::IntegerVector(),
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector(),
            Rcpp::Named("value_map") = Rcpp::List()
        );
    }
    
    // Get number of rows from first non-null column
    for (int i = 0; i < n_cols && n_rows == 0; i++) {
        if (data[i] != R_NilValue) {
            Rcpp::RObject col = data[i];
            n_rows = Rf_length(col);
        }
    }
    
    if (n_rows == 0) {
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = Rcpp::IntegerVector(),
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector(),
            Rcpp::Named("value_map") = Rcpp::List()
        );
    }
    
    // Convert incomparables to set for fast lookup - optimized
    std::unordered_set<std::string> incomp_set;
    incomp_set.reserve(incomparables.size()); // Pre-allocate capacity
    for (int i = 0; i < incomparables.size(); i++) {
        std::string val = Rcpp::as<std::string>(incomparables[i]);
        if (!case_sensitive) {
            std::transform(val.begin(), val.end(), val.begin(), ::tolower);
        }
        incomp_set.insert(std::move(val)); // Move instead of copy
    }
    
    // Create value-to-rows mapping with better performance
    std::unordered_map<std::string, std::vector<int>> value_to_rows;
    value_to_rows.reserve(n_rows); // Pre-allocate expected capacity
    
    // Process each column
    for (int col = 0; col < n_cols; col++) {
        Rcpp::RObject column = data[col];
        
        if (column == R_NilValue) continue;
        
        // Handle different column types - optimized
        if (TYPEOF(column) == STRSXP) {
            Rcpp::CharacterVector char_col = Rcpp::as<Rcpp::CharacterVector>(column);
            int col_size = static_cast<int>(char_col.size());
            int max_rows = (n_rows < col_size) ? n_rows : col_size;
            
            // Pre-allocate string for case conversion to avoid repeated allocations
            std::string val;
            val.reserve(50); // Reserve space for typical string length
            
            for (int row = 0; row < max_rows; row++) {
                if (char_col[row] == NA_STRING) continue;
                
                val = Rcpp::as<std::string>(char_col[row]);
                if (val.empty()) continue;
                
                if (!case_sensitive) {
                    std::transform(val.begin(), val.end(), val.begin(), ::tolower);
                }
                
                // Skip incomparable values - check after case conversion
                if (incomp_set.find(val) != incomp_set.end()) continue;
                
                value_to_rows[val].push_back(row);
            }
        } else if (TYPEOF(column) == REALSXP) {
            Rcpp::NumericVector num_col = Rcpp::as<Rcpp::NumericVector>(column);
            int col_size = static_cast<int>(num_col.size());
            int max_rows = (n_rows < col_size) ? n_rows : col_size;
            
            // Optimize: Use direct numeric keys instead of string conversion
            std::unordered_map<double, std::vector<int>> numeric_to_rows;
            for (int row = 0; row < max_rows; row++) {
                if (Rcpp::NumericVector::is_na(num_col[row])) continue;
                double val = num_col[row];
                numeric_to_rows[val].push_back(row);
            }
            
            // Convert to string map only for values that appear multiple times
            for (const auto& pair : numeric_to_rows) {
                if (pair.second.size() > 1) {
                    std::string str_val = std::to_string(pair.first);
                    value_to_rows[str_val] = pair.second;
                }
            }
            
        } else if (TYPEOF(column) == INTSXP) {
            Rcpp::IntegerVector int_col = Rcpp::as<Rcpp::IntegerVector>(column);
            int col_size = static_cast<int>(int_col.size());
            int max_rows = (n_rows < col_size) ? n_rows : col_size;
            
            // Optimize: Use direct integer keys instead of string conversion
            std::unordered_map<int, std::vector<int>> int_to_rows;
            for (int row = 0; row < max_rows; row++) {
                if (int_col[row] == NA_INTEGER) continue;
                int val = int_col[row];
                int_to_rows[val].push_back(row);
            }
            
            // Convert to string map only for values that appear multiple times
            for (const auto& pair : int_to_rows) {
                if (pair.second.size() > 1) {
                    std::string str_val = std::to_string(pair.first);
                    value_to_rows[str_val] = pair.second;
                }
            }
        }
    }
    
    // Initialize Union-Find
    UnionFind uf(n_rows);
    
    // Union rows that share values - optimized
    for (const auto& pair : value_to_rows) {
        const std::vector<int>& rows = pair.second;
        if (rows.size() < 2) continue;  // Need at least 2 rows to form a group
        
        // Union all rows that share this value - optimize by reducing union operations
        int root = rows[0];
        for (size_t i = 1; i < rows.size(); i++) {
            uf.union_sets(root, rows[i]);
        }
    }
    
    // Assign group IDs - optimized with pre-allocation
    std::unordered_map<int, int> root_to_group;
    root_to_group.reserve(n_rows / 10); // Estimate for number of unique roots
    std::vector<int> group_ids(n_rows, 0);
    std::vector<int> temp_group_sizes;
    temp_group_sizes.reserve(n_rows / 10); // Pre-allocate group sizes vector
    int next_group_id = 1;
    
    // First pass: identify roots and count group sizes
    std::unordered_map<int, int> root_counts;
    for (int i = 0; i < n_rows; i++) {
        int root = uf.find(i);
        root_counts[root]++;
    }
    
    // Second pass: assign group IDs only to groups meeting minimum size
    for (int i = 0; i < n_rows; i++) {
        int root = uf.find(i);
        
        if (root_counts[root] >= min_group_size) {
            if (root_to_group.find(root) == root_to_group.end()) {
                root_to_group[root] = next_group_id++;
                temp_group_sizes.push_back(root_counts[root]);
            }
            group_ids[i] = root_to_group[root];
        } else {
            group_ids[i] = 0;  // Singleton or small group
        }
    }
    
    // Create value mapping for output
    Rcpp::List value_map;
    std::vector<std::string> map_names;
    
    for (const auto& pair : value_to_rows) {
        if (pair.second.size() >= 2) {  // Only include values that create groups
            map_names.push_back(pair.first);
            Rcpp::IntegerVector row_vector(static_cast<int>(pair.second.size()));
            // Copy values and convert to 1-based indexing for R
            for (size_t i = 0; i < pair.second.size(); i++) {
                row_vector[static_cast<int>(i)] = pair.second[i] + 1;
            }
            value_map.push_back(row_vector);
        }
    }
    value_map.names() = map_names;
    
    return Rcpp::List::create(
        Rcpp::Named("group_ids") = group_ids,
        Rcpp::Named("n_groups") = next_group_id - 1,
        Rcpp::Named("group_sizes") = temp_group_sizes,
        Rcpp::Named("value_map") = value_map
    );
}

//' Fast Multi-Column Group ID Assignment for Numeric Data
//'
//' Optimized version for numeric-only columns. Much faster than string-based grouping.
//' 
//' @param data List of numeric/integer vectors
//' @param min_group_size Minimum group size to assign group ID
//' @return List with group_ids, n_groups, group_sizes
// [[Rcpp::export]]
Rcpp::List multi_column_group_numeric_cpp(const Rcpp::List& data,
                                          int min_group_size = 1) {
    
    int n_rows = 0;
    int n_cols = data.size();
    
    if (n_cols == 0) {
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = Rcpp::IntegerVector(),
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector()
        );
    }
    
    // Get number of rows from first non-null column
    for (int i = 0; i < n_cols && n_rows == 0; i++) {
        if (data[i] != R_NilValue) {
            Rcpp::RObject col = data[i];
            n_rows = Rf_length(col);
        }
    }
    
    if (n_rows == 0) {
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = Rcpp::IntegerVector(),
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector()
        );
    }
    
    // Fast numeric value-to-rows mapping
    std::unordered_map<double, std::vector<int>> double_to_rows;
    std::unordered_map<int, std::vector<int>> int_to_rows;
    
    // Process each column - optimized for numeric types only
    for (int col = 0; col < n_cols; col++) {
        Rcpp::RObject column = data[col];
        
        if (column == R_NilValue) continue;
        
        if (TYPEOF(column) == REALSXP) {
            Rcpp::NumericVector num_col = Rcpp::as<Rcpp::NumericVector>(column);
            int col_size = std::min(static_cast<int>(num_col.size()), n_rows);
            
            for (int row = 0; row < col_size; row++) {
                if (Rcpp::NumericVector::is_na(num_col[row])) continue;
                double val = num_col[row];
                double_to_rows[val].push_back(row);
            }
        } else if (TYPEOF(column) == INTSXP) {
            Rcpp::IntegerVector int_col = Rcpp::as<Rcpp::IntegerVector>(column);
            int col_size = std::min(static_cast<int>(int_col.size()), n_rows);
            
            for (int row = 0; row < col_size; row++) {
                if (int_col[row] == NA_INTEGER) continue;
                int val = int_col[row];
                int_to_rows[val].push_back(row);
            }
        }
    }
    
    // Initialize Union-Find
    UnionFind uf(n_rows);
    
    // Union rows that share numeric values - faster without string conversion
    for (const auto& pair : double_to_rows) {
        const std::vector<int>& rows = pair.second;
        if (rows.size() >= 2) {
            for (size_t i = 1; i < rows.size(); i++) {
                uf.union_sets(rows[0], rows[i]);
            }
        }
    }
    
    for (const auto& pair : int_to_rows) {
        const std::vector<int>& rows = pair.second;
        if (rows.size() >= 2) {
            for (size_t i = 1; i < rows.size(); i++) {
                uf.union_sets(rows[0], rows[i]);
            }
        }
    }
    
    // Assign group IDs efficiently
    std::unordered_map<int, int> root_to_group;
    std::vector<int> group_ids(n_rows, 0);
    std::vector<int> group_sizes;
    int next_group_id = 1;
    
    // Count group sizes
    std::unordered_map<int, int> root_counts;
    for (int i = 0; i < n_rows; i++) {
        root_counts[uf.find(i)]++;
    }
    
    // Assign group IDs
    for (int i = 0; i < n_rows; i++) {
        int root = uf.find(i);
        
        if (root_counts[root] >= min_group_size) {
            if (root_to_group.find(root) == root_to_group.end()) {
                root_to_group[root] = next_group_id++;
                group_sizes.push_back(root_counts[root]);
            }
            group_ids[i] = root_to_group[root];
        }
    }
    
    return Rcpp::List::create(
        Rcpp::Named("group_ids") = group_ids,
        Rcpp::Named("n_groups") = next_group_id - 1,
        Rcpp::Named("group_sizes") = group_sizes
    );
}

//' Ultra-Fast Entity Resolution for Large Numeric Datasets
//'
//' Completely different algorithm optimized for millions of records.
//' Uses value-based grouping instead of row-based Union-Find.
//' 
//' @param data List of numeric/integer vectors
//' @param min_group_size Minimum group size to assign group ID
//' @return List with group_ids, n_groups, group_sizes
// [[Rcpp::export]]
Rcpp::List ultra_fast_group_numeric_cpp(const Rcpp::List& data,
                                        int min_group_size = 1) {
    
    int n_rows = 0;
    int n_cols = data.size();
    
    if (n_cols == 0) {
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = Rcpp::IntegerVector(),
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector()
        );
    }
    
    // Get number of rows
    for (int i = 0; i < n_cols && n_rows == 0; i++) {
        if (data[i] != R_NilValue) {
            n_rows = Rf_length(data[i]);
        }
    }
    
    if (n_rows == 0) {
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = Rcpp::IntegerVector(),
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector()
        );
    }
    
    // Step 1: Build row->values mapping efficiently
    std::vector<std::vector<int64_t>> row_values(n_rows);
    std::unordered_map<int64_t, std::vector<int>> value_to_rows;
    
    // Collect all values for each row
    for (int col = 0; col < n_cols; col++) {
        Rcpp::RObject column = data[col];
        if (column == R_NilValue) continue;
        
        if (TYPEOF(column) == REALSXP) {
            Rcpp::NumericVector num_col = Rcpp::as<Rcpp::NumericVector>(column);
            for (int row = 0; row < std::min(static_cast<int>(num_col.size()), n_rows); row++) {
                if (Rcpp::NumericVector::is_na(num_col[row])) continue;
                
                int64_t val = static_cast<int64_t>(num_col[row]);
                row_values[row].push_back(val);
                value_to_rows[val].push_back(row);
            }
        } else if (TYPEOF(column) == INTSXP) {
            Rcpp::IntegerVector int_col = Rcpp::as<Rcpp::IntegerVector>(column);
            for (int row = 0; row < std::min(static_cast<int>(int_col.size()), n_rows); row++) {
                if (int_col[row] == NA_INTEGER) continue;
                
                int64_t val = static_cast<int64_t>(int_col[row]);
                row_values[row].push_back(val);
                value_to_rows[val].push_back(row);
            }
        }
    }
    
    // Step 2: Use much simpler Union-Find on a pre-filtered set
    // Only process rows that share at least one value with another row
    std::vector<int> active_rows;
    for (const auto& pair : value_to_rows) {
        if (pair.second.size() > 1) { // Value appears in multiple rows
            for (int row : pair.second) {
                active_rows.push_back(row);
            }
        }
    }
    
    // Remove duplicates and sort
    std::sort(active_rows.begin(), active_rows.end());
    active_rows.erase(std::unique(active_rows.begin(), active_rows.end()), active_rows.end());
    
    // If no shared values, everyone is in their own group
    if (active_rows.empty()) {
        std::vector<int> group_ids(n_rows, 0);
        return Rcpp::List::create(
            Rcpp::Named("group_ids") = group_ids,
            Rcpp::Named("n_groups") = 0,
            Rcpp::Named("group_sizes") = Rcpp::IntegerVector()
        );
    }
    
    // Step 3: Fast Union-Find only on active rows
    UnionFind uf(n_rows);
    
    // Union rows that share values - much faster with pre-filtering
    for (const auto& pair : value_to_rows) {
        const std::vector<int>& rows = pair.second;
        if (rows.size() < 2) continue;
        
        // Union all rows that share this value
        for (size_t i = 1; i < rows.size(); i++) {
            uf.union_sets(rows[0], rows[i]);
        }
    }
    
    // Step 4: Assign group IDs efficiently
    std::unordered_map<int, int> root_to_group;
    std::vector<int> group_ids(n_rows, 0);
    std::vector<int> group_sizes;
    int next_group_id = 1;
    
    // Count group sizes only for active rows
    std::unordered_map<int, int> root_counts;
    for (int row : active_rows) {
        root_counts[uf.find(row)]++;
    }
    
    // Assign group IDs
    for (int row : active_rows) {
        int root = uf.find(row);
        
        if (root_counts[root] >= min_group_size) {
            if (root_to_group.find(root) == root_to_group.end()) {
                root_to_group[root] = next_group_id++;
                group_sizes.push_back(root_counts[root]);
            }
            group_ids[row] = root_to_group[root];
        }
    }
    
    return Rcpp::List::create(
        Rcpp::Named("group_ids") = group_ids,
        Rcpp::Named("n_groups") = next_group_id - 1,
        Rcpp::Named("group_sizes") = group_sizes
    );
}
