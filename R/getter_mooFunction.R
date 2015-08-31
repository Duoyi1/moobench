#' @export
getParamSet = function(f) {
  UseMethod("getParamSet")
}

#' @export
getParamSet.mooFunction = function(f)
  attributes(f)$param.set
  