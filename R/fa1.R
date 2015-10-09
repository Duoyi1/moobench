#' FA1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be three.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be three.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [47]
#' 
#' @aliases fa1 FA1
generateFA1 = function(in.dim = 3L, out.dim = 3L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 3L)
    stop("FA1 supports only in.dim = 3.")
  if (out.dim != 3L)
    stop("FA1 supports only out.dim = 3.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "fa1",
    id = sprintf("fa1-%id-%id", in.dim, out.dim),
    fun = fa1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of fa1
fa1 = function(x) {
  f1 = (1 - exp(- 4 * x[1L])) / (1 - exp(-4))
  f2 = (x[2L] + 1) * (1 - (f1 / (x[2L] + 1))^0.5)
  f3 = (x[3L] + 1) * (1 - (f1 / (x[3L] + 1))^0.1)
  return(c(f1, f2, f3))
}