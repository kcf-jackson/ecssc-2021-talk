# is_connected <- function(x1, x2, lonlat = TRUE) {
#   dist_matrix <- raster::pointDistance(x1, x2, lonlat = lonlat)
#   min(dist_matrix) == 0
# }

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
