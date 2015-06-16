#' ZDT1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references ZDT Reference Einfuegen
#' 
#' @export
#' 
generateZDT1 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("ZDT1 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "zdt1",
    id = sprintf("zdt1-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(zdt1, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


zdt1 = function(x) {
  f1 = x[1]
  g = 1 + 9 * mean(x[-1])
  f2 = g * (1 - sqrt(f1 / g))
  return(c(f1, f2))
}
