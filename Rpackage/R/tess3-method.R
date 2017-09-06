#' Sampler class
#'
#'
#' @export
Method <- function(name = "A method", hypothesis.testing.method = NULL,
                   nickname = NULL) {
  structure(list(name = name,
                 hypothesis.testing.method = hypothesis.testing.method,
                 nickname = nickname), class = "Method")
}



################################################################################
# Methods

#' fit
#'
#' @export
fit <- function(m, dat, reuse = FALSE, ...){
  UseMethod("fit")
}

#' name
#'
#' @export
name <- function(m){
  UseMethod("name")
}

#' Run the association scan method
#'
#' @export
run <- function(m, dat, ...){
  UseMethod("run")
}

#' Clean all estimates
#'
#' @export
clean <- function(m) {
  UseMethod("clean")
}

#' Title
#'
#'
#' @export
print.Method <- function(x) {
  str(x)
}


#' @export
name.Method <- function(m) {

  if (!is.null(m$nickname)) {
    return(m$nickname)
  }
  if (!is.null(m$name)) {
    paste0(m$name, "|",name(m$hypothesis.testing.method))
  } else {
    "A method"
  }
}
