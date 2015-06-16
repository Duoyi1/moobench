#' ZDT6 test function generator.
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
generateZDT6 = function(in.dim = 10, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("ZDT6 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "zdt6",
    id = sprintf("zdt6-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(zdt6, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


zdt6 = function(x, out.dim) {
  f1 = 1 - exp(-4 * x[1]) * sin(6 * pi * x[1])^6
  g = 1 + 9 * mean(x[-1])^0.25
  f2 = g * (1 - (f1 / g)^2)
  return(c(f1, f2))
}
