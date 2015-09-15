#' SK2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be four.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [61]
#' 
#' @aliases sk2 SK2
generateSK2 = function(in.dim = 4L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 4L)
    stop("SK2 supports only in.dim = 4.")
  if (out.dim != 2L)
    stop("SK2 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "sk2",
    id = sprintf("sk2-%id-%id", in.dim, out.dim),
    fun = sk2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of sk2
sk2 = function(x) {
  f1 = -(x[1L] - 2)^2 - (x[2L] + 3)^2 - (x[3L] - 5)^2 - (x[4L] - 4)^2 + 5
  f2 = sum(sin(x)) / (1 + sum(x^2) / 100)
  return(c(f1, f2))
}