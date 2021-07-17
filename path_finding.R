source("streets_to_graph.R")
system.time({
  street_graph <- build_graph_from_streets(
    melb_lines$geometry,
    grepl(x = melb_lines$other_tags,
          pattern = "\"oneway\"=>\"yes\"")
  )
})

source("queue.R")
source("binary_search.R")

Char <- as.character

dist <- \(x, y) sqrt(sum((x - y)^2))

manhattan <- \(x, y) sum(abs(x - y))

adjacent_cache <- function(graph) {
  g_sorted <- graph |> arrange(from)
  counts <- g_sorted %>% group_by(from) %>% summarise(count = n())
  order_hashmap <- cumsum(counts$count)
  
  get_adjacent <- function(x) {
    index <- binary_search(x, counts$from)
    if (index == -1) {
      return(numeric(0))
    } else if (index == 1) {
      adja_rows <- 1:order_hashmap[index]
    } else {
      adja_rows <- (order_hashmap[index-1] + 1):order_hashmap[index]
    }
    g_sorted[adja_rows, ]$to
  }
  
  return(get_adjacent)
}

a_star_pathfinding <- function(start, end, graph, get_adjacent) {
  coordinates <- graph$nodes
    
  heuristic <- \(a, b) manhattan(coordinates[a, ], coordinates[b, ])
  graph_cost <- \(a, b) dist(coordinates[a, ], coordinates[b, ])

  if (missing(get_adjacent)) {
    get_adjacent <- adjacent_cache(graph)
  }
  
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
    if (frontier$length() > 10000) browser()
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

adjacent_fun <- adjacent_cache(street_graph$edges)

profvis::profvis({
  path <- a_star_pathfinding(80, 101, street_graph, adjacent_fun)
})
 

# get_adjacent <- \(x, graph) graph[graph[, 1] == x, 2]
# tmp <- as.matrix(street_graph$graph)
# profvis::profvis({path <- a_star_pathfinding(14, 4, tmp, nodes)})


# g <- street_graph$graph
# gm <- as.matrix(g)
# g_sorted <- g |> arrange(from)
# microbenchmark::microbenchmark(
#   g[g$from == 111, ]$to, 
#   gm[gm[, 1] == 111, 2],
#   g_sorted[g_sorted$from == 111, ]$to
# )
