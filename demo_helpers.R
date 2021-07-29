#' @export
demo_script <- function(file, ...) {
  res <- Display(readLines(file), ...)
  res$start()
  res
}

#' @examples
#' res <- Display(readLines("presentation/demo_1.R"))
#' res$start()
#' res$stop()
Display <- function(lines, line_delay = 1, ...) {
  lines <- lines
  pointer <- 1
  interrupt <- FALSE
  
  show_line <- function() {
    is_call <- function(x, names) {
      any(deparse1(x[[1]]) %in% names)
    }
    argument_of <- function(x) {
      if (length(x) >= 2) x[[2]] else NULL
    }
    # cat("Number of lines remaining:", length(lines))
    
    x <- lines[pointer]
    pointer <<- pointer + 1
    if (is_directive(x)) {
      input <- parse(text = substring(x, 3))[[1]]
      if (is_call(input, "Pause")) {
        pause(argument_of(input))
      } else if (is_call(input, "ConfirmContinue")) {
        confirm_continue(argument_of(input))
      } else {
        eval(input)  
      }
    } else if (is_separator(x)) {
      display_line(trim_separator(x), char_delay = 0)
    } else {
      display_line(x, ...)
    }
  }
  
  start <- function() {
    interrupt <<- FALSE
    step()
  }
  
  step <- function() {
    if (pointer <= length(lines) && !interrupt) {
      show_line()
      later::later(step, line_delay)
    } 
  }
  
  reset <- function(n = 1) {
    pointer <<- n
  }
  
  stop <- function() {
    interrupt <<- TRUE
  }
  
  pause <- function(seconds) {
    Sys.sleep(seconds)
  }
  
  confirm_continue <- function(x) {
    if (missing(x) || is.null(x)) {
      x <- "[INPUT] Continue(T/F)? "
    }
    res <- as.logical(readline(x))
    interrupt <<- !res
  }
  
  list(lines = lines, show_line = show_line, interrupt = interrupt,
       start = start, step = step, stop = stop, reset = reset)
}

display_line <- function(line0, char_delay = 0.05) {
  for (i in 1:nchar(line0)) {
    text <- paste0(substring(line0, 1, i))
    rstudioapi::sendToConsole(text, execute = F, focus = F)
    Sys.sleep(char_delay)
  }
  rstudioapi::sendToConsole(line0, focus = F)
  rstudioapi::sendToConsole("", execute = F, focus = F)
}

is_directive <- function(x) {
  substring(x, 1, 2) == "#!"
}

is_separator <- function(x) {
  if (nchar(x) < 4) return(FALSE)
  last_four <- substring(x, nchar(x) - 3, nchar(x))
  last_four == "====" || last_four == "----"
}

trim_separator <- function(x) {
  substring(x, 1, options("width"))
}
