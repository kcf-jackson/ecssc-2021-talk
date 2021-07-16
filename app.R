# This is the main app

#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "dom"))
#! load_script("https://unpkg.com/leaflet@1.6.0/dist/leaflet.css")
#! load_script("https://unpkg.com/leaflet@1.6.0/dist/leaflet.js")
#! load_script("styles.css")

#! load_library("websocket")
#! load_library("dom")
#! load_library("d3")
# div(id = "demo_map", style = "width: 800px; height: 600px") %>%
#   render()
div(id = "demo_map", style = "width: 1200px; height: 800px") %>% 
  render()
c <- Array

# Set up a leaflet map
bounding_points <- c(c(-37.6134, 144.6924), c(-38.0267, 145.5068))
map <- L$map("demo_map")$fitBounds(bounding_points)

L$tileLayer(
  "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  list(attribution = "Thanks to <a href=\"http://openstreetmap.org\">OpenStreetMap</a> community",
       maxZoom = 20)
)$addTo(map)

map$on('click', function(ev) {
  latlng <- map$mouseEventToLatLng(ev$originalEvent)
  console::log(latlng$lat + ', ' + latlng$lng)
})

# TODO: Add panel. Point. Multi-points. 
# Lasso = fast many points.
