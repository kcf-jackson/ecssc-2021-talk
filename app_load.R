library(sketch)
source("app_interact.R")

#' Load map app and open WebSocket connection for interactivity
#' @param env An environment to store the variables collected from the browser.
#' This is used to avoid modifying the user's global environment.
#' @export
start_map_server <- function(env) {
  in_handler <- function(msg) {
    x <- jsonlite::fromJSON(msg)
    if (x$type == "data") {
      env$selection <- x$message
      message("Data have been fetched from the browser successfully.")
    } else {
      print(x)
    }
  }
  
  out_handler <- function(x) {
    x |> 
      compile_exprs(rules = basic_rules(), deparsers = dp("basic", "macro")) |>
      map(~list(type = "command", message = .x)) |>
      map(~jsonlite::toJSON(.x, auto_unbox = TRUE))
  }
  
  # Initial WebSocket connection
  handle <- websocket$new(in_handler = in_handler, out_handler = out_handler)
  handle$startServer()
  
  # Start the app
  source_r("app.R", debug = F)
  
  return(handle)
}


#' Create a helper function to send command to the browser
#' @param x A command to be transpiled to JavaScript and run at the browser.
#' @param message TRUE / FALSE; whether to show the message to be sent to the 
#' browser. This is useful for debugging.
#' @export
send <- function(x, message = F, n = 1) {
  msg <- out_handler_with_env(deparse1(substitute(x), collapse = "\n"), 
                              env = parent.frame(n))
  if (message) print(msg)
  handle$ws$send(msg)
}

#' The variable name is generated using the `hygienic_name` function
out_handler_with_env <- function(x_pqse5mxv, env) {
  local_env <- rlang::env_clone(env)
  local_env$x_pqse5mxv <- x_pqse5mxv
  
  expression(
    compile_exprs(x_pqse5mxv, 
                  rules = basic_rules(), 
                  deparsers = dp("basic", "auto", "macro"))
  ) |> 
    eval(envir = local_env) |>
    map(~list(type = "command", message = .x)) |>
    map(~jsonlite::toJSON(.x, auto_unbox = TRUE))
}

#' Create a randomly generated variable name
hygienic_name <- function() {
  sample(c(letters, 0:9), 8) |> paste(collapse = "")
}
