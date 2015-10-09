#' DGO1 test function generator.
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
#' @aliases dgo1 DGO1
generateDGO1 = function(in.dim = 1L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("DGO1 supports only in.dim = 1.")
  if (out.dim != 2L)
    stop("DGO1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -10, upper = 13)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "dgo1",
    id = sprintf("dgo1-%id-%id", in.dim, out.dim),
    fun = dgo1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of dgo1
dgo1 = function(x) {
  f1 = sin(x)
  f2 = sin(x + 0.7)
  return(c(f1, f2))
}