source("DS_queue.R")
Char <- as.character
dist <- \(x, y) sqrt(sum((x - y)^2))
manhattan <- \(x, y) sum(abs(x - y))


#' Pathfinding using the A*-algorithm
#' @param start An integer; the node id.
#' @param end An integer; the node id.
#' @param graph The graph, with both edge-list and node-list.
#' @param get_adjacent (Optional) A function for finding the neighbors of a node.
#' @export
find_path <- function(start, end, graph) {
  coordinates <- graph$nodes
  get_adjacent <- graph$get_adjacent
  
  heuristic <- \(a, b) manhattan(coordinates[a, ], coordinates[b, ])
  graph_cost <- \(a, b) dist(coordinates[a, ], coordinates[b, ])
  
  frontier <- PriorityQueue()
  frontier$put(start, 0)
  came_from <- list()
  cost_so_far <- list()
  came_from[[Char(start)]] <- NA
  cost_so_far[[Char(start)]] <- 0

  while (frontier$length() > 0) {
    current <- frontier$get()
    
    if (current == end) 
      break
    
    # debug(get_adjacent)    
    for (next_entry in get_adjacent(current)) {
      new_cost <- cost_so_far[[Char(current)]] + graph_cost(current, next_entry)
      if (!Char(next_entry) %in% names(cost_so_far) ||
          new_cost < cost_so_far[[Char(next_entry)]]) {
        cost_so_far[[Char(next_entry)]] <- new_cost
        priority <- new_cost + heuristic(next_entry, end)
        frontier$put(next_entry, priority)
        came_from[[Char(next_entry)]] <- current
      }
    }
    # if (frontier$length() > 10000) browser()
  }
  
  list(path = reconstruct_path(came_from, start, end),
       cost = cost_so_far)
}

reconstruct_path <- function(came_from, start, end) {
  current <- end
  path <- c()
  while (current != start) {
    path <- append(path, current)
    current <- came_from[[Char(current)]]
  }
  path <- append(path, start)
  rev(path)
}
