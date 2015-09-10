


#' @rdname get_pareto
#' @export
getParetoSet = function(f, n) {
  UseMethod("getParetoSet")
}

#' @export
getParetoSet.mooFunction = function(f, n)
  attributes(f)$paretoSet


#' @rdname get_pareto
#' @export
getParetoFront = function(f, n) {
  UseMethod("getParetoFront")
}

#' @export
getParetoFront.mooFunction = function(f, n) {
  set = getParetoSet(f, n)
  front = t(apply(set, 1, f))
  front = front[order(front[, 1L]), ]
  front
}