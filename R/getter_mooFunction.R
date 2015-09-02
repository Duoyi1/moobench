#' Wrappers to get attributes from a mooFunction
#'
#' \code{getParamSet} Simple wrapper for \code{.attributes(f)$param.set}, returns a \code{list}.
#' \code{getName} Simple wrapper for \code{.attributes(f)$name}, returns a \code{character(1)}.
#' \code{getId} Simple wrapper for \code{.attributes(f)$id}, returns a \code{character(1)}.
#' \code{getInDim} Simple wrapper for \code{.attributes(f)$in.dim}, returns a \code{integer(1)}.
#' \code{getOutDim} Simple wrapper for \code{.attributes(f)$out.dim}, returns a \code{integer(1)}.
#'
#' @param f [\code{function}] \cr
#'   A mooFunction.

#' @export
getParamSet = function(f) {
  UseMethod("getParamSet")
}

#' @export
getParamSet.mooFunction = function(f)
  attributes(f)$param.set


#' @export
getName = function(f) {
  UseMethod("getName")
}

#' @export
getName.mooFunction = function(f)
  attributes(f)$name


#' @export
getId = function(f) {
  UseMethod("getId")
}

#' @export
getId.mooFunction = function(f)
  attributes(f)$id


#' @export
getInDim = function(f) {
  UseMethod("getInDim")
}

#' @export
getInDim.mooFunction = function(f)
  attributes(f)$in.dim


#' @export
getOutDim = function(f) {
  UseMethod("getOutDim")
}

#' @export
getOutDim.mooFunction = function(f)
  attributes(f)$out.dim
  