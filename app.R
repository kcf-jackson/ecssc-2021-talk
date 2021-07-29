# This is the app interface

# Set-up =======================================================================
#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "dom"))

#! load_script("https://unpkg.com/leaflet@1.6.0/dist/leaflet.css")
#! load_script("https://unpkg.com/leaflet@1.6.0/dist/leaflet.js")
#! load_script("styles.css")

#! load_library("websocket")
#! load_library("dom")
#! load_library("d3")
#! load_script("app_helpers.R")
#! load_script("app_animate_route.R")


# Helpers and variables ========================================================
selection <- c()
map <- NULL
leaflet_layers <- Namespace()
active <- NULL


# Interface ====================================================================
# Create a div to display the map 
render(div(id = "demo_map", style = "width: 98vw; height: 97vh"))

# Set up a leaflet map
bounding_points <- c(c(-37.6134, 144.6924), c(-38.0267, 145.5068))
map <- L$map("demo_map", list(preferCanvas = FALSE))$
  fitBounds(bounding_points)

# Add tile to the map
attribution <- "Thanks to <a href=\"http://openstreetmap.org\">OpenStreetMap</a> community"
L$tileLayer(
  "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  list(attribution = attribution, maxZoom = 19)
)$addTo(map)

# Set up interaction
map$on('click', function(ev) {
  latlng <- map$mouseEventToLatLng(ev$originalEvent)
  selection$push(latlng)
})

# TODO: Add panel. Point. Multi-points. 
# Lasso = fast many points.
