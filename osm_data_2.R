#-------------------------------------------------------------------------------
# Please run part 1 before part 2 (demo 2)
# 2. Suburbs ===================================================================
suburb <- melb_mpolygons |>
  filter(type == "boundary", boundary == "administrative", admin_level == 6)

# # Draw boundaries
# suburb_points <- suburb$geometry |> map(as_latlng_matrix)
# suburb_points |> walk_delay(polygon)
# 
# # Add center and labels
# suburb_center <- suburb_points |> map(centroid)
# suburb_center |> walk_delay(points, color = "green", radius = 3, fillOpacity = 1)
# suburb_center |> walk2_delay(suburb$name, \(x, y) text(x, y))
# 
# # Compute graph and add connecting lines

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
      bindTooltip(.data(polygon_name), list(direction = 'top'))$
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
source("SIM_utils.R")
set.seed(123)
# Find graph from list of boundaries
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
next_counts <- initial_counts
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
