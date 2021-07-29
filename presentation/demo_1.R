# Start an interactive leaflet map session
source("app_load.R")
JS_env <- new.env()
handle <- start_map_server(JS_env)

# Demo 1 =======================================================================
#! Pause(3)

# Plot a point
source("SC_utils.R")
sample_point <- melb_points[1, ] |> get_points_matrix()
sample_point
#! Pause(2)
points(sample_point)
#! ConfirmContinue()


# Plot a line
sample_street <- melb_lines[13, ] |> get_points_matrix()
head(sample_street)
#! Pause(2)
lines(sample_street, weight = 10)
#! ConfirmContinue()


# Zoom to location
zoom_to(sample_street[1, ], 14)
#! Sys.sleep(5)
zoom_to(c(-37.81656, 144.9584), 12)


# Plot a boundary
melb_CBD_boundary <- melb_mpolygons |> 
  filter(name == "City of Melbourne") |>
  get_points_matrix()
head(melb_CBD_boundary)
#! Pause(2)
polygon(melb_CBD_boundary)
#! ConfirmContinue()


# Plot 50 sample restaurants
is_restaurant <- grepl("restaurant", melb_points$other_tags)
restaurants <- melb_points[is_restaurant, ]
#! Pause(2)
restaurants |> 
  slice_sample(n = 50) |> 
  get_points_matrix() |>
  points()
#! ConfirmContinue()

# Plot 50 sample restaurants in Melbourne CBD
CBD_restaurants <- restaurants |>
  filter(in_bound(geometry, melb_CBD_boundary))
#! Pause(2)
CBD_restaurants |>
  slice_sample(n = 50) |>
  get_points_matrix() |>
  points(color = "brown")
#! ConfirmContinue()

# Get data from the browser
# Once fetched, the data will be available at `JS_env`.
# I will click on the map now.
#! ConfirmContinue()
collect()
JS_env$selection

# Select three other points to show it actually works
#! ConfirmContinue()

collect()
JS_env$selection
#! ConfirmContinue()


# Stop the app
handle$stopServer()
