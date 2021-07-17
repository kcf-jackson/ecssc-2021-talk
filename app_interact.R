points <- function(x, ...) {
  options <- list(...)
  if (is.matrix(x)) {
    for (i in 1:nrow(x)) {
      datum <- x[i, ]
      send(L$circleMarker(.data(datum, digits = NA),
                          .data(options))$addTo(map))
      Sys.sleep(0.01)
    }
  } else {
    send(L$circleMarker(.data(x, digits = NA),
                        .data(options))$addTo(map))
  }
}

lines <- function(x, ...) {
  options <- list(...)
  send(L$polyline(.data(x, digits = NA), .data(options))$addTo(map))  
}

polygon <- function(x, ...) {
  options <- list(...)
  send(L$polygon(.data(x, digits = NA), .data(options))$addTo(map))  
}

#' Send a request to the browser to send back the data stored in a variable
collect <- function() {
  send(ws$send(
    JSON::stringify(list(type = "data", message = selection))
  ))
}
