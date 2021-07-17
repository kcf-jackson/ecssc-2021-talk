# This is the app interface

# Set-up =======================================================================
#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "dom"))

#! load_script("https://unpkg.com/leaflet@1.6.0/dist/leaflet.css")
#! load_script("https://unpkg.com/leaflet@1.6.0/dist/leaflet.js")
#! load_script("styles.css")

#! load_library("websocket")
#! load_library("dom")
#! load_library("d3")


# Helpers and variables ========================================================
c <- Array
selection <- c()
map <- NULL


# Interface ====================================================================
# Create a div to display the map 
render(div(id = "demo_map", style = "width: 1200px; height: 800px"))

# Set up a leaflet map
bounding_points <- c(c(-37.6134, 144.6924), c(-38.0267, 145.5068))
map <- L$map("demo_map")$fitBounds(bounding_points)

# Add tile to the map
attribution <- "Thanks to <a href=\"http://openstreetmap.org\">OpenStreetMap</a> community"
L$tileLayer(
  "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  list(attribution = attribution,maxZoom = 20)
)$addTo(map)

# Set up interaction
map$on('click', function(ev) {
  latlng <- map$mouseEventToLatLng(ev$originalEvent)
  selection$push(latlng)
})

# TODO: Add panel. Point. Multi-points. 
# Lasso = fast many points.
