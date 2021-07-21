#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"))

c <- Array

Namespace <- function() {
  layers <- c()
  id <- c()
  
  add <- function(layer, layer_id) {
    layers$push(layer)
    if (!layer_id || layer_id == "") {
      layer_id <- "layer_" %+% id$length
    }
    id$push(layer_id)
    TRUE
  }
  
  pop <- function() {
    id$pop()
    layers$pop()
  }
  
  get <- function(target_id) {
    i <- 0
    while (i < id$length) {
      if (id[i] == target_id) {
        return(layers[i])
      }
      i <- i + 1
    }
    NULL
  }
  
  length <- function() layers$length
  
  last <- function() layers[layers$length - 1]
  
  list(layers = layers, id = id, add = add, get = get,
       length = length, last = last)
}

# # Usage
# env <- Namespace()
# env$add('hi')
# console::log(env$layers)
# console::log(env$id)
# 
# env$add('hi-2')
# console::log(env$layers)
# console::log(env$id)
# 
# console::log(env$get("layer_0"))
# console::log(env$get("layer_1"))
# console::log(env$get("layer_999"))
# 
# console::log(env$length())
# console::log(env$last())
# 
# env$add('hi-3', 'id-3')
# console::log(env$layers)
# console::log(env$id)
