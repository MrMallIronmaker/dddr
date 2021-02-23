
#' @export
focus_point <- function(initials, directions, accept_behind = T, accept_single = F) {
  # angle between is dot divided by length.

  # ensure directions is normalized.
  directions <- direction(directions)

  sym_sum <- matrix(c(
    sum(directions$x * directions$x), sum(directions$y * directions$x), sum(directions$z * directions$x),
    sum(directions$x * directions$y), sum(directions$y * directions$y), sum(directions$z * directions$y),
    sum(directions$x * directions$z), sum(directions$y * directions$z), sum(directions$z * directions$z)),
    nrow=3, ncol=3
  )

  target <- sum(directions * dot(directions, initials) - initials)

  solution <- solve(sym_sum - length(directions)*diag(3), c(target$x, target$y, target$z))

  vector3(solution[[1]], solution[[2]], solution[[3]])
}
