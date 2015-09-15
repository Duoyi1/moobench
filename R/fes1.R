#' FES1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [49]
#' 
#' @aliases fes1 FES1
generateFES1 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("FES1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "fes1",
    id = sprintf("fes1-%id-%id", in.dim, out.dim),
    fun = fes1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of fes1
fes1 = function(x) {
  n = length(x)
  f1 = sum(abs(x - exp(((1:n) / n)^2) / 3)^0.5)
  f2 = sum((x - 0.5 * cos(10 * pi * (1:n) / n) - 0.5)^2)
  return(c(f1, f2))
}