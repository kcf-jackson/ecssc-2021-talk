# Map plotting primitives

#' @param x A pair / matrix of coordinates
#' @export
points <- function(x, id = NULL, ...) {
  options <- list(...)
  if (is.matrix(x)) {
    for (i in 1:nrow(x)) {
      datum <- x[i, ]
      send(leaflet_layers$add(
        L$circleMarker(.data(datum, digits = NA),
                       .data(options))$addTo(map),
        .data(id, null = "null")
      ))
      Sys.sleep(0.01)
    }
  } else {
    send(leaflet_layers$add(
      L$circleMarker(.data(x, digits = NA),
                     .data(options))$addTo(map),
      .data(id, null = "null")
    ))
  }
}

#' @param x A matrix of coordinates
#' @export
lines <- function(x, id = NULL, ...) {
  options <- list(...)
  send(leaflet_layers$add(
    L$polyline(.data(x, digits = NA), .data(options))$addTo(map),
    .data(id, null = "null")
  ))  
}

#' @param x A matrix of coordinates
#' @export
polygon <- function(x, id = NULL, ...) {
  options <- list(...)
  send(leaflet_layers$add(
    L$polygon(.data(x, digits = NA), .data(options))$addTo(map),
    .data(id, null = "null")
  ))
}

#' Send a request to the browser to send back the data stored in a variable
#' @export
collect <- function() {
  send(ws$send(
    JSON::stringify(list(type = "data", message = selection))
  ))
}

#' @param x A character string; the element / reference id.
#' @export
select <- function(x) {
  send(active <- leaflet_layers$get(.data(x)))  
}

#' @param x A matrix of coordinates
#' @export
animate_along <- function(x, step_size = 0.0001, start = TRUE, ...) {
  options <- list(...)
  start_coord <- x[1, ]
  
  send(Routes(
    .data(x, digits = NA),
    .data(step_size),
    (function() {
      start_marker <- L$circleMarker(
        .data(start_coord, digits = NA),
        .data(options)
      )$addTo(map)
      return(function(latlng) { 
        # console::log(latlng)
        start_marker$setLatLng(latlng) 
      })
    })() 
  )[0]$begin())
  
  # if (start) {
  #   Sys.sleep(0.001)
  #   send(active[0]$begin())
  # }
}

#' #' @export
#' remove <- function(x, all = TRUE) {
#'   x <- paste0(".", x)
#'   if (all) {
#'     send(Array::from(select_doms(.data(x)))$
#'            map(x %=>% x$remove()))
#'   } else {
#'     send(select_dom(.data(x))$remove())
#'   }
#' }


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
