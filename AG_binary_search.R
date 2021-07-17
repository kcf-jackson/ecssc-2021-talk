binary_search <- function(x, data) {
  L <- 1
  R <- length(data)
  while (L <= R) {
    m <- floor((L + R) / 2)
    if (x > data[m]) 
      L <- m + 1
    else if (x < data[m]) 
      R <- m - 1
    else 
      return(m)
  }
  
  # stop(sprintf("The element %d cannot be found.", x))
  return(-1)
}
