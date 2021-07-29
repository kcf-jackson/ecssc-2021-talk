#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"))

Route <- function(start, end, step_size, render = console::log, 
                  milliseconds = 200) {
  alpha <- 0
  speed <- step_size / dist(start, end)
  callback <- NULL
  handle <- NULL
    
  bind <- function(callback_on_clear) {
    callback <<- callback_on_clear   
    TRUE
  }
  
  step <- function() {
    alpha <<- Math::min(alpha + speed, 1)
    list(
      position = add_two_array(
        constant_times_array(1 - alpha, start),
        constant_times_array(alpha, end)
      ),
      alpha = alpha
    )
  }
  
  begin <- function() {
    then <- performance$now()
    
    animate <- function() {
      timer <- requestAnimationFrame(animate)
      
      now <- performance$now()
      elapsed <- now - then
      if (elapsed > milliseconds) {
        then <<- now - (elapsed %% milliseconds)

        # main animation loop
        res <- step()
        if (res$alpha >= 1) {
          window$cancelAnimationFrame(timer)
          if (callback) callback()
        }
        render(res$position)
      }
      return(timer)
    }
    
    handle <<- animate()
    handle
  }
  
  list(begin = begin, bind = bind)
}

dist <- function(start, end) {
  Math::sqrt((start[0] - end[0])^2 + (start[1] - end[1])^2)
}

add_two_array <- function(arr1, arr2) arr1$map(\(x, i) arr1[i] + arr2[i])

constant_times_array <- function(k, xs) xs$map(x %=>% k * x)

# # Usage
# start <- Array(1, 1)
# end <- Array(10, 10)
# res <- Route(start, end, 1)
# res$begin()
 

Routes <- function(coordinates_array, step_size, render, milliseconds) {
  route_list <- coordinates_array$
    map(function(x, i) {
      if (i < last_index(coordinates_array)) {
        start <- coordinates_array[i]
        end <- coordinates_array[i + 1]
        s <- Route$new(start, end, step_size, render, milliseconds)
        return(s)
      }
      NULL
    })$
    filter(x %=>% x != NULL)
  
  # This step must be splitted from the above, because the linking of two objects
  # only happens after the list of objects are created.
  route_list <- route_list$
    map(function(x, i) {
      if (i < last_index(route_list)) {
        next_route <- route_list[i + 1]
        x$bind(function() next_route$begin())
        return(x)
      }
      NULL
    })$
    filter(x %=>% x!= NULL)
  
  route_list
}

last_index <- function(x, k = 1) x$length - k

# # Usage
# coordinates <- Array(Array(1, 1), Array(3, 4), Array(-4, 8), Array(-10, 5))
# res <- Routes(coordinates, 1)
# res[0]$begin()
