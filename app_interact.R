# Map plotting primitives

#' @export
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

#' @export
lines <- function(x, ...) {
  options <- list(...)
  send(L$polyline(.data(x, digits = NA), .data(options))$addTo(map))  
}

#' @export
polygon <- function(x, ...) {
  options <- list(...)
  send(L$polygon(.data(x, digits = NA), .data(options))$addTo(map))  
}

#' Send a request to the browser to send back the data stored in a variable
#' @export
collect <- function() {
  send(ws$send(
    JSON::stringify(list(type = "data", message = selection))
  ))
}


#===============================================================================
# Helpers ======================================================================

#' @export
show_street <- function(street, street_type, n = 100, ...) {
  filtered_street <- street |> dplyr::filter(highway == street_type)
  num_street <- nrow(filtered_street)
  num_shown <- min(num_street, n)
  message(sprintf("Number of street type '%s': %d (showing %d)", 
                  street_type, num_street, num_shown))
  
  ind <- sample(num_street, num_shown)
  for (i in ind) {
    filtered_street$geometry[[i]] |>
      as_latlng_matrix() |> 
      lines(...)
    Sys.sleep(0.001)
  }  
}

# # Usage
# show_street(street, "motorway", Inf)
# show_street(street, "motorway_link", Inf, color = "red")
# show_street(street, "trunk", 1000, color = "brown")
# show_street(street, "primary", 1e3, color = "black")
# show_street(street, "secondary", 1e3, color = "darkgreen")
# show_street(street, "tertiary", 10, color = "yellow")
