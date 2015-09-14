#' Print a mooFunction.
#'
#' @param x [\code{function}] \cr 
#'  A \code{\link{mooFunction}}.
#' 
#' @export
print.mooFunction = function(x) {
  catf("%dD %dM %s Function", getInDim(x), getOutDim(x), getName(x))
  cat("  ParamSet:\n")
  print(getParamSet(x))
}