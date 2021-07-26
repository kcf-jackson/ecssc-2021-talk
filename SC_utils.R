#' Find the nearest node on a graph 
#' 
#' @note The distance is computed using the Exclidean distance.
#' 
#' @param x A lat-lng pair of coordinates.
#' @param x A matrix of coordinates; the node-list of a graph.
#' 
#' @return The lat-lng coordinates of the nearest node.
#' 
#' @export
nearest_node <- function(x, nodes, k = 1) {
  d <- pdist::pdist(x, nodes)@dist
  ind <- head(order(d), k)
  nodes[ind, ]
}
 

#' Check if a matrix of coordinates are in bound
#' 
#' @param p A lat-lng pair of coordinates.
#' @param bounds A matrix of coordinates defining the boundary.
#' 
#' @return TRUE / FALSE.
#' 
#' @export
#' 
#' @references https://stackoverflow.com/questions/217578/how-can-i-determine-whether-a-2d-point-is-within-a-polygon
is_point_in_polygon <- function(p, bounds) {
  is_inside <- FALSE
  x_range <- range(bounds[,1])
  y_range <- range(bounds[,2])
  
  # If outside bounding box, then return FALSE
  px <- p[1]
  py <- p[2]
  if (px < x_range[1] || px > x_range[2] || 
      py < y_range[1] || py > y_range[2]) {
    return(FALSE)
  }
  
  i <- 1
  j <- nrow(bounds)
  while (i <= nrow(bounds)) {
    if (( (bounds[i, 2] > py) != (bounds[j, 2] > py) ) &&
        px < (bounds[j, 1] - bounds[i, 1]) * (py - bounds[i, 2]) / (bounds[j, 2] - bounds[i, 2]) + bounds[i,1]) {
      is_inside <- !is_inside 
    }
    j <- i
    i <- i + 1
  }
  is_inside
}


#' Count the number of keywords in a list of tags
#' 
#' @param keywords A character string; the query to be passed to a `grepl` call.
#' @param tags A character vector.
#' 
#' @return An integer.
#' 
#' @export
count_x_in_y <- function(keywords, tags, ...) {
  sum(grepl(keywords, tags, ...))  
}
 

#' Check if a list of geometry objects are within some bounds
#' @return A logical vector.
in_bound <- function(geometry_list, bounds) {
  geometry_list %>% 
    map_lgl(function(x) {
      x |> 
        as_latlng_matrix() |>
        is_point_in_polygon(bounds)
    })
}


#' Extract the geometry point matrix from an entry / record
#' @param x A 'sf' dataframe row.
#' @return A numeric matrix.
get_points_matrix <- function(x) {
  x$geometry %>% 
    purrr::map(~as_latlng_matrix(.x)) %>% 
    do.call(rbind, .)
}


#' Walk with delay in between function calls
#' @param xs A list.
#' @param f A function.
#' @param ... Optional arguments to `f`.
#' @param delay Time in seconds to sleep.
#' @return Nothing
walk_delay <- function(xs, f, ..., delay = 0.05) {
  for (x in xs) {
    f(x, ...)
    Sys.sleep(delay)
  }
}


#' Walk2 with delay in between function calls
#' @param xs A list.
#' @param ys A list.
#' @param f A function.
#' @param ... Optional arguments to `f`.
#' @param delay Time in seconds to sleep.
#' @return Nothing
walk2_delay <- function(xs, ys, f, ..., delay = 0.05) {
  for (i in seq_along(xs)) {
    f(xs[[i]], ys[[i]], ...)
    Sys.sleep(delay)
  }
}




#=============================================================================== 
is_connected <- function(x1, x2, lonlat = TRUE) {
  dist_matrix <- pdist::pdist(x1, x2)@dist
  min(dist_matrix) == 0
}

build_graph_from_boundary <- function(boundary_list, ...) {
  n <- length(boundary_list)
  adja_matrix <- matrix(data = 0, nrow = n, ncol = n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      bdy_i <- boundary_list[[i]]
      bdy_j <- boundary_list[[j]]
      adja_matrix[i, j] <- is_connected(bdy_i, bdy_j, ...)
    }
  }
  adja_matrix
}

full_adja <- function(x) {
  x + t(x)
}

centroid <- function(m0) {
  x <- m0[,1]
  y <- m0[,2]
  xi <- head(x, -1)
  xip1 <- tail(x, -1)
  yi <- head(y, -1)
  yip1 <- tail(y, -1)
  
  A <- 0.5 * sum(xi*yip1 - xip1*yi)
  cx <- sum((xi + xip1) * (xi*yip1 - xip1*yi)) / (6 * A)
  cy <- sum((yi + yip1) * (xi*yip1 - xip1*yi)) / (6 * A)
  c(cx, cy)
}

as_latlng_matrix <- function(x) {
  res <- as.matrix(x)
  if (any(range(res[, 1]) < -90) || any(range(res[, 1]) > 90)) {
    return(res[, 2:1])
  }
  res
}
