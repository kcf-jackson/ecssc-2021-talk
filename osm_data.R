#===============================================================================
# OSM data © OpenStreetMap contributors
# OSM data is available under the Open Database Licence.
# OSM copyright and licence: https://www.openstreetmap.org/copyright
#===============================================================================


# Set-up =======================================================================
# remotes::install_github("kcf-jackson/sketch", "experiment")
library(osmextract)
library(sf)
library(purrr)
library(dplyr)
library(magrittr)

# Load data / map features (See `readme.md` for instructions to download the data)
file <- "./data/melbourne.osm.pbf"

# Get a summary of what's available
feature_counts <- st_layers(file)

# Import the features one-by-one
melb_lines <- oe_read(file) # default is 'lines'
melb_points <- oe_read(file, "points")
melb_mlines <- oe_read(file, "multilinestrings")
melb_mpolygons <- oe_read(file, "multipolygons")
melb_relations <- oe_read(file, "other_relations")

# Start an interactive leaflet map session
source("app_load.R")
JS_env <- new.env()   # This variable is used to collect data from the browser later
handle <- start_map_server(JS_env)


# Analysis =====================================================================
# Inspect the data 
View(melb_melb_mpolygons)
View(melb_lines)

# Get the suburbs and streets
suburb <- melb_mpolygons |>
  filter(type == "boundary", boundary == "administrative", admin_level == 6)
street <- melb_lines |> filter(!is.na(highway))

# Use a routing service
# Approach 1 - Turn the street data into a graph and use a path-finding algorithm
# This allows you to do live changes on the graph and re-routing
source("SC_streets_to_graph.R")
# Takes about 5 mins
system.time({
  street_graph <- build_graph_from_streets(
    street_geometry = melb_lines$geometry,
    one_way = grepl(x = melb_lines$other_tags, pattern = "\"oneway\"=>\"yes\"")
  )
})
nodes <- street_graph$nodes

# Using the graph for some basic computation
source("AG_path_finding.R")
source("SC_utils.R")
points(nodes[60, ])
points(nodes[100, ], color = "red")

adjacent_fun <- adjacent_cache(street_graph$edges)
route <- find_path(60, 100, street_graph, adjacent_fun)
lines(nodes[route$path, ])


#-------------------------------------------------------------------------------
# Approach 2 - use the OSRM service --------------------------------------------
# This does not allow graph changes and re-routing, but it has very good performance.
# See docker set-up instruction at https://github.com/Project-OSRM/osrm-backend
# Also, see the R binding at https://github.com/riatelab/osrm
find_path_2 <- function(x, y, nodes) {
  # osrm takes lng-lat as inputs
  src <- rev(nodes[x, ])
  dst <- rev(nodes[y, ])
  osrm::osrmRoute(src, dst, returnclass = "sf", overview = "full", 
                  osrm.server = "http://127.0.0.1:5000/")
}
route <- find_path_2(60, 100, nodes)
lines(route$geometry[[1]] |> as_latlng_matrix(), color = "brown")




# Extract suburbs ==============================================================
suburb <- melb_mpolygons %>% 
  filter(type == "boundary", boundary == "administrative", admin_level == 6)
boundary <- suburb$geometry
boundary_name <- suburb$name

# Set up handle on the browser side
len_boundary <- length(boundary)
send(boundary_handle <- Array(.data(len_boundary)))
send(center_handle <- Array(.data(len_boundary)))
send(text_handle <- Array(.data(len_boundary)))

# Add boundaries on the map
for (i in seq_along(boundary)) {
  polygon_data <- boundary[[i]] %>% as.matrix() %>% extract(, 2:1)
  polygon_name <- boundary_name[[i]]
  send(
    boundary_handle[.data(i) - 1] <- L$polygon(.data(polygon_data))$
      bindTooltip(.data(polygon_name))$
      addTo(map)
  )
  Sys.sleep(0.05)
}

# Add counter at the boundary center
boundary_center <- boundary %>% 
  map(~.x %>% as.matrix() %>% centroid())
for (i in seq_along(boundary)) {
  circle_data <- rev(boundary_center[[i]])
  circle_name <- boundary_name[[i]]
  # hotfix to what seems to be a data issue (city of Bayside) ----------------
  if (boundary_name[i] == "City of Bayside") {
    circle_data <- c(-37.94168765706131, 145.01781512391975)
  }
  # End of hotfix ------------------------------------------------------------
  send(
    center_handle[.data(i) - 1] <- L$circleMarker(
      .data(circle_data), 
      list(radius = 20, fillOpacity = 0.8)
    )$
      bindTooltip("0", list(permanent = TRUE, 
                            direction = "center",
                            className = "my-text-label"))$
      addTo(map)
  )
  Sys.sleep(0.05)
}


# Simulate movement on the map =================================================
# Find graph from list of boundaries
source("spatial_computation.R")
source("utils.R")
graph <- boundary %>% 
  map(as.matrix) %>% 
  build_graph_from_boundary() %>% 
  full_adja()

# Initialise a random transition matrix on the graph
# Right stochastic matrix, i.e. rows sum to 1
transition_prob <- as_stochastic(graph * runif(361))
rowSums(transition_prob)  # Verify

# Simulation
# Given a count for each region, simulate n step of markov chain
initial_counts <- sample(100:1000, length(boundary))

# Update the map based on the initial counts
for (i in 1:len_boundary) {
  # tooltip <- paste(boundary_name[[i]], ":", initial_counts[i])
  color <- initial_counts[i] / max(initial_counts)
  # send(
  #   boundary_handle[.data(i)-1]$
  #     setTooltipContent(.data(tooltip))
  # )
  send(
    boundary_handle[.data(i)-1]$
      setStyle(list(fillColor = d3.interpolateOranges(.data(color))))
  )
  
  count_number <- as.character(initial_counts[i])
  send(
    center_handle[.data(i) - 1]$
      setTooltipContent(.data(count_number))
  )
}

# Perform the simulation
n_iter <- 50
for (i in 1:n_iter) {
  # Simulate using Markov Chain
  next_counts <- seq_along(next_counts) %>% 
    map(function(i) one_step(i, transition_prob, next_counts[i])) %>% 
    do.call(c, .) %>% 
    table() %>% 
    as.numeric()
  # Update the map based on the simulated counts
  for (i in 1:len_boundary) {
    color <- next_counts[i] / max(next_counts)
    send(
      boundary_handle[.data(i)-1]$
        setStyle(list(fillColor = d3.interpolateOranges(.data(color))))
    )
    count_number <- as.character(next_counts[i])
    send(
      center_handle[.data(i) - 1]$
        setTooltipContent(.data(count_number))
    )
  }
  Sys.sleep(0.5)
}


#-------------------------------------------------------------------------------
# Experiment

# Draw a route (example)
tmp_src = as.numeric(melb_points$geometry[1][[1]])
tmp_dst = as.numeric(melb_points$geometry[2][[1]])
tmp = as.numeric(melb_points$geometry[1000][[1]])

# Add markers
tmp_src <- rev(tmp_src)
tmp_dst <- rev(tmp_dst)
tmp <- rev(tmp)
send(L$circleMarker(.data(tmp_src), list(radius = 4))$addTo(map))
send(L$circleMarker(.data(tmp_dst), list(radius = 4))$addTo(map))
send(L$circleMarker(.data(tmp), list(radius = 4))$addTo(map))

# Find and plot routes
tmp_src <- rev(tmp_src)
tmp_dst <- rev(tmp_dst)
tmp <- rev(tmp)

route_simplified <- osrmRoute(src = tmp_src, dst = tmp_dst, returnclass = "sf", 
                             osrm.server = "http://127.0.0.1:5000/")
route_latlngs <- route$geometry[[1]] %>% as.matrix() %>% extract(, 2:1)
send(L$polyline(.data(route_latlngs, digits = NA), list(color = 'yellow'))$addTo(map))

route_latlngs <- route_simplified$geometry[[1]] %>% as.matrix() %>% extract(, 2:1)
send(L$polyline(.data(route_latlngs, digits = NA), list(color = 'green'))$addTo(map))

route2 <- osrmRoute(src = tmp_src, dst = tmp, returnclass = "sf", 
                   overview = "full", osrm.server = "http://127.0.0.1:5000/")
route2_simplified <- osrmRoute(src = tmp_src, dst = tmp, returnclass = "sf", 
                               osrm.server = "http://127.0.0.1:5000/")
route_latlngs <- route2$geometry[[1]] %>% as.matrix() %>% extract(, 2:1)
send(L$polyline(.data(route_latlngs, digits = NA), list(color = 'red'))$addTo(map))

route_latlngs <- route2_simplified$geometry[[1]] %>% as.matrix() %>% extract(, 2:1)
send(L$polyline(.data(route_latlngs, digits = NA), list(color = 'pink'))$addTo(map))




# Extract suburbs ==============================================================
library(dplyr)
tags <- melb_points$other_tags
postcode_ind <- which(!is.na(melb_points$place) & melb_points$place != "city")
postcode_areas <- melb_points[postcode_ind, ] |> filter(!is.na(name))
postcode_areas |> View()

suburb_names <- postcode_areas$name
latlngs <- postcode_areas$geometry
coord_to_string <- function(x) {
  rev(x) |> as.character() |> paste(collapse = ", ") %>% 
    paste("c(", ., ")", sep = "")
}

handle$sketch_mode
L$circleMarker(.macro(data, tmp), list(radius = 20))$addTo(leaflet_map)
# Expand leaflet functions by one argument for piping
# Add .data(x, ...) function to sketch
# 

# Need a way to pass variable to the browser
for (i in seq_along(latlngs)) {
  glue::glue("L$circleMarker({coord_to_string(latlngs[[i]])}, list(radius = 5))$
             bindTooltip(
                \"{suburb_names[[i]]}\", 
                list(permanent = TRUE,
                     className = 'my-label', 
                     offset = c(0, 0))
             )$
             addTo(leaflet_map)") |>
    handle$out_handler() |> 
    walk(~handle$ws$send(.x))
  Sys.sleep(0.01)
}


for (i in seq_along(latlngs)) {
  glue::glue("L$circleMarker({coord_to_string(latlngs[[i]])}, list(radius = 5))$
             bindTooltip(
                \"{suburb_names[[i]]}\", 
                list(permanent = TRUE,
                     className = 'my-label', 
                     offset = c(0, 0))
             )$
             addTo(leaflet_map)") |>
    handle$out_handler() |> 
    walk(~handle$ws$send(.x))
  Sys.sleep(0.01)
}




"L$circleMarker(c(-37.8141705, 144.9655616), list(radius = 200))" %>% 
  handle$out_handler() %>% unlist() %>% 
  handle$ws$send()

