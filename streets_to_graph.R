source("optimisation.R")
source("graph.R")

#' Build a graph from street data
#' @param street_geometry
#' @param one_way
build_graph_from_streets <- function(street_geometry, one_way) {
  street_matrix <- street_geometry |> map(as.matrix)
  total <- do.call(rbind, street_matrix)
  total <- total[, 2:1]
  
  nodes <- unique(total)
  
  row_counts <- street_matrix |> map(nrow)
  header_ids <- cumsum(row_counts) + 1
  
  one_way <- purrr::map2(one_way, row_counts, ~rep(.x, .y)) %>% 
    do.call(c, .)
  
  same_location <- function(x, y) all(x == y)
  
  find_cid <- function(centry) find_x_pos_in_y(centry, nodes) + 1
  
  # Main
  network_graph <- graph(1000)
  
  # Start at the second row because the first node can never be duplicated
  pid <- 1
  crow <- 2
  header_pointer <- 1
  pb <- txtProgressBar(1, nrow(nodes), initial = 1, style = 3)
  for (node_cid in 2:nrow(nodes)) {
    setTxtProgressBar(pb, node_cid)
    node_centry <- nodes[node_cid, ]
    centry <- total[crow, ]
    
    while (!same_location(node_centry, centry)) {
      cid <- find_cid(centry)
      # An edge requires two nodes, so skip the edge building if current node is 
      # the first node of the street
      if (crow == header_ids[header_pointer]) {
        header_pointer <- header_pointer + 1
      } else {
        network_graph$add(pid, cid, list())  
        if (!one_way[crow]) {
          network_graph$add(cid, pid, list())  
        } 
      }
      # cat(node_cid, "x", cid, " ", sep = "")
      # cat(glue::glue("G({pid}, {cid})"), "\n")
      
      crow <- crow + 1
      centry <- total[crow, ]
      pid <- cid
    }
    
    # Same location
    cid <- node_cid
    if (crow == header_ids[header_pointer]) {
      header_pointer <- header_pointer + 1
    } else {
      network_graph$add(pid, cid, list())
      if (!one_way[crow]) {
        network_graph$add(cid, pid, list())  
      }
    }
    # cat(node_cid, "-", cid, " ", sep = "")
    # cat(glue::glue("G({pid}, {cid})"), "\n")
    
    crow <- crow + 1
    pid <- cid
  }
  
  g <- network_graph$get()
  list(edges = g$graph, edges_attributes = g$attributes, nodes = nodes)
}
