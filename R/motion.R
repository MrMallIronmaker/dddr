
#' @export
approx <- function(x, y=NULL, ...) {
  UseMethod("approx", y)
}

#' @method approx dddr_vector3
#' @export
approx.dddr_vector3 <- function(x, y, ...) {
  list(
    x = x,
    y = vector3(
      x = stats::approx(x, y$x, ...)$y,
      y = stats::approx(x, y$y, ...)$y,
      z = stats::approx(x, y$z, ...)$y,
    )
  )
}

#' @method approx dddr_quat
#' @export
approx.dddr_quat <- function(x, y, ...) {
  stop("Not implemented")
}

#' @method approx default
#' @export
approx.default <- function(x, y, ...) {
  stats::approx(x, y, ...)
}
