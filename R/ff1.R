#' FF1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [50]
#' 
#' @aliases ff1 FF1
generateFF1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("FF1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("FF1 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -4, upper = 4)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "ff1",
    id = sprintf("ff1-%id-%id", in.dim, out.dim),
    fun = ff1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of ff1
ff1 = function(x) {
  f1 = 1 - exp(-(x[1L] - 1)^2 - (x[2L] + 1)^2)
  f2 = 1 - exp(-(x[1L] + 1)^2 - (x[2L] - 1)^2)
  return(c(f1, f2))
}