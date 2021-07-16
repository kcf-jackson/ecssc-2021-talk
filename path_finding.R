system.time({
  street_graph <- build_graph_from_streets(
    melb_lines$geometry, 
    grepl(x = melb_lines$other_tags, 
          pattern = "\"oneway\"=>\"yes\"")
  )
})

# a_star_pathfinding <- function(start, end, graph, coordinates) {
#   compute_cost <- function(a, cost_to_a, prev = NULL) {
#     h <- manhattan(coordinates[a, ], coordinates[end, ])
#     g <- cost_to_a
#     f <- h + g
#     list(id = a, h = h, g = g, f = f, prev = prev)
#   }
#   
#   open <- list()
#   close <- list()
# 
#   current <- compute_cost(start, 0)
#   while (current$id != end) {
#     for (vertex in get_adjacent(current$id, graph)) {
#       if (not_in(vertex, open) && not_in(vertex, close)) {
#         cost_to_vertex <- dist(coordinates[current$id, ],
#                                coordinates[vertex, ])
#         open <- append(open, compute_cost(vertex, cost_to_vertex, current$id))
#         if ()
#       }
#     }
#   }
# }
# 
# print_node <- function(x) {
#   cat("%d | %d | %d | %d | %d", node$id, node$g, node$h, node$f, 
#       ifelse(is.null(node$prev), -1, node$prev)
# }
# 
# not_in <- function(a, bs) {
#   !(a %in% purrr::map_dbl(bs, ~.x$id))
# }

source("queue.R")

Char <- as.character

dist <- \(x, y) sqrt(sum((x - y)^2))

manhattan <- \(x, y) sum(abs(x - y))

get_adjacent <- \(x, graph) graph[graph$from == x, ]$to  

a_star_pathfinding <- function(start, end, graph, coordinates) {
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
    
    for (next_entry in get_adjacent(current, graph)) {
      new_cost <- cost_so_far[[Char(current)]] + graph_cost(current, next_entry)
      if (!Char(next_entry) %in% names(cost_so_far) ||
          new_cost < cost_so_far[[Char(next_entry)]]) {
        cost_so_far[[Char(next_entry)]] <- new_cost
        priority <- new_cost + heuristic(next_entry, end)
        frontier$put(next_entry, priority)
        came_from[[Char(next_entry)]] <- current
      }
    }
    print(frontier$length())
  }
  
  list(path = reconstruct_path(came_from), cost = cost_so_far)
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

