#' FES3 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be four.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [49]
#' 
#' @aliases fes3 FES3
generateFES3 = function(in.dim = 30L, out.dim = 4L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 4L)
    stop("FES3 supports only out.dim = 4.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "fes3",
    id = sprintf("fes3-%id-%id", in.dim, out.dim),
    fun = fes3,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of fes3
fes3 = function(x) {
  n = length(x)
  f1 = sum(abs(x - exp(((1:n) / n)^2) / 3)^0.5)
  f2 = sum(abs(x - sin((1:n) - 1)^2 * cos((1:n) - 1)^2)^0.5)
  f3 = sum(abs(x - 0.25 * cos((1:n) - 1) * cos(2 * (1:n) - 2) - 0.5)^0.5)
  f4 = sum((x - 0.5 * sin(1000 * pi * (1:n) / n) - 0.5)^2)
  return(c(f1, f2, f3, f4))
}