#' FES2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be three.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [49]
#' 
#' @aliases fes2 FES2
generateFES2 = function(in.dim = 30L, out.dim = 3L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 3L)
    stop("FES2 supports only out.dim = 3.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "fes2",
    id = sprintf("fes2-%id-%id", in.dim, out.dim),
    fun = fes2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of fes2
fes2 = function(x) {
  n = length(x)
  f1 = sum((x - 0.5 * cos(10 * pi * (1:n) / n) - 0.5)^2)
  f2 = sum(abs(x - sin((1:n) - 1)^2 * cos((1:n) - 1)^2)^0.5)
  f3 = sum(abs(x - 0.25 * cos((1:n) - 1) * cos(2 * (1:n) - 2) - 0.5)^0.5)
  return(c(f1, f2, f3))
}