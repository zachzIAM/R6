#' @export
"+.R6" <- function(e1, e2) {
  f <- .subset2(e1, ".__enclos_env__")$private$`+`
  if (is.function(f)) {
    f(e2)
  } else {
    stop(paste0("No '+' method defined for R6 class '", class(e1)[1]), "'")
  }
}

#' @export
"-.R6" <- function(e1, e2) {
  f <- .subset2(e1, ".__enclos_env__")$private$`-`
  if (is.function(f)) {
    f(e2)
  } else {
    stop(paste0("No '-' method defined for R6 class '", class(e1)[1]), "'")
  }
}

#' @export
"*.R6" <- function(e1, e2) {
  f <- .subset2(e1, ".__enclos_env__")$private$`*`
  if (is.function(f)) {
    f(e2)
  } else {
    stop(paste0("No '*' method defined for R6 class '", class(e1)[1]), "'")
  }
}

#' @export
"/.R6" <- function(e1, e2) {
  f <- .subset2(e1, ".__enclos_env__")$private$`/`
  if (is.function(f)) {
    f(e2)
  } else {
    stop(paste0("No '/' method defined for R6 class '", class(e1)[1]), "'")
  }
}

#' @export
"^.R6" <- function(e1, e2) {
  f <- .subset2(e1, ".__enclos_env__")$private$`^`
  if (is.function(f)) {
    f(e2)
  } else {
    stop(paste0("No '^' method defined for R6 class '", class(e1)[1]), "'")
  }
}
