#' Wrappers to get attributes from a mooFunction
#'
#' \code{getParamSet} Simple wrapper for \code{attributes(f)$param.set}, returns a \code{\link[ParamHelpers]{ParamSet}} . \cr
#' \code{getName} Simple wrapper for \code{attributes(f)$name}, returns a \code{character(1)}. \cr
#' \code{getId} Simple wrapper for \code{attributes(f)$id}, returns a \code{character(1)}. \cr
#' \code{getInDim} Simple wrapper for \code{attributes(f)$in.dim}, returns a \code{integer(1)}. \cr
#' \code{getOutDim} Simple wrapper for \code{attributes(f)$out.dim}, returns a \code{integer(1)}. \cr
#'
#' @param f [\code{function}] \cr
#'   A \code{\link{mooFunction}}.
#' @name getter_mooFunction

#' @rdname getter_mooFunction
#' @export
getParamSet = function(f) {
  UseMethod("getParamSet")
}

#' @export
getParamSet.mooFunction = function(f)
  attributes(f)$param.set


#' @rdname getter_mooFunction
#' @export
getName = function(f) {
  UseMethod("getName")
}

#' @export
getName.mooFunction = function(f)
  attributes(f)$name


#' @rdname getter_mooFunction
#' @export
getId = function(f) {
  UseMethod("getId")
}

#' @export
getId.mooFunction = function(f)
  attributes(f)$id


#' @rdname getter_mooFunction
#' @export
getInDim = function(f) {
  UseMethod("getInDim")
}

#' @export
getInDim.mooFunction = function(f)
  attributes(f)$in.dim


#' @rdname getter_mooFunction
#' @export
getOutDim = function(f) {
  UseMethod("getOutDim")
}

#' @export
getOutDim.mooFunction = function(f)
  attributes(f)$out.dim
  