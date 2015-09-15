#' DGO2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be one.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [46]
#' 
#' @aliases dgo2 DGO2
generateDGO2 = function(in.dim = 1L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("DGO2 supports only in.dim = 1.")
  if (out.dim != 2L)
    stop("DGO2 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -9, upper = 9)
  
  paretoSet = NULL
  
  mooFunction(
    name = "dgo2",
    id = sprintf("dgo2-%id-%id", in.dim, out.dim),
    fun = dgo2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of dgo2
dgo2 = function(x) {
  f1 = x^2
  f2 = 9 - sqrt(81 - x^2)
  return(c(f1, f2))
}