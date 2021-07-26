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


# Basic analysis and visualisations ============================================
# 1. Streets ===================================================================
# a. Inspect the data 
View(melb_melb_mpolygons)
View(melb_lines)

# b. Get the streets
street <- melb_lines |> filter(!is.na(highway))

# c. Use a routing service
#-------------------------------------------------------------------------------------
# Approach 1 - Turn the street data into a graph and use a path-finding algorithm ----
# This allows you to do live changes on the graph and re-routing
source("SC_streets_to_graph.R")
# Takes about 5-8 mins
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
route <- find_path(60, 100, street_graph)
lines(nodes[route$path, ])

# Reduce the set of streets to the main roads for better routing
source("AG_path_finding_multilevel.R")
filtered_street <- street |>
  filter(!highway %in% c("footway", "residential", "service"))
reduced_graph <- simplify_graph(street_graph, filtered_street$geometry)
route <- find_path_multilevel(60, 100, reduced_graph)
lines(nodes[route$path, ], color = "orange")


#-------------------------------------------------------------------------------
# Approach 2 - use the OSRM service --------------------------------------------
# It does not seem to allow graph changes and re-routing, but it has very good performance.
# See docker set-up instruction at https://github.com/Project-OSRM/osrm-backend
# Also, see the R binding at https://github.com/riatelab/osrm
# Continue with the remaining part of this section after starting the OSRM server with docker.
find_path_2 <- function(id_1, id_2, nodes) {
  # osrm takes lng-lat as inputs
  src <- rev(nodes[id_1, ])
  dst <- rev(nodes[id_2, ])
  res <- osrm::osrmRoute(src, dst, returnclass = "sf", overview = "full", 
                         osrm.server = "http://127.0.0.1:5000/")
  res$geometry[[1]] |> as_latlng_matrix()
  
}
route <- find_path_2(60, 100, nodes)
lines(route, color = "brown")
#-------------------------------------------------------------------------------

# d. Animate along a route
animate_along(nodes[route$path, ], step_size = 0.0001, color = "orange")

# Larger scale 
# https://www.vicroads.vic.gov.au/traffic-and-road-use/road-network-and-performance/road-use-and-performance
# The peak hourly traffic for the busiest road (at 8am) is about 10-12K
set.seed(123)
m <- 1000
start <- sample(nrow(nodes), m)
end <- sample(nrow(nodes), m)
routes <- purrr::map(seq(m), ~find_path_2(start[.x], end[.x], nodes))
# It is recommended to use `find_path_2` over `find_path_multilevel` for far-apart 
# nodes because the former is written in C++ while the latter is written in R with 
# little optimisation performed.

points(nodes[start, ], radius = 5)
points(nodes[end, ], color = "red", radius = 5)
for (route in routes) {
  lines(route, opacity = 0.5)
  Sys.sleep(0.01)
}

for (route in routes) {
  animate_along(route, step_size = 0.0001, 
                color = "brown", fill = TRUE,
                fillOpacity = 0.8, radius = 5)
  Sys.sleep(0.01)
}
