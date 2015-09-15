#' JOS1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [53]
#' 
#' @aliases jos1 JOS1
generateJOS1 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("JOS1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim)
  
  paretoSet = NULL
  
  mooFunction(
    name = "jos1",
    id = sprintf("jos1-%id-%id", in.dim, out.dim),
    fun = jos1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of jos1
jos1 = function(x) {
  n = length(x)
  f1 = sum(x^2 / n)
  f2 = sum((x - 2)^2 / n)
  return(c(f1, f2))
}