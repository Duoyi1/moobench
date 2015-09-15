#' QV1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [59]
#' 
#' @aliases fes2 FES2
generateFES2 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("QV1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -5.12, upper = 5.12)
  
  paretoSet = NULL
  
  mooFunction(
    name = "qv1",
    id = sprintf("qv1-%id-%id", in.dim, out.dim),
    fun = qv1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of qv1
qv1 = function(x) {
  n = length(x)
  f1 = sum((x^2 - 10 * cos(2 * pi * x) + 10) / n)^0.25
  f2 = sum(((x - 1.5)^2 - 10 * cos(2 * pi * (x - 1.5)) + 10) / n)^0.25
  return(c(f1, f2))
}