#' ZDT4 test function generator.
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
generateZDT4 = function(in.dim = 10L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("ZDT4 support only out.dim = 2.")
  
  lower.bounds = c(0, rep(-5, in.dim - 1))
  upper.bounds = c(1, rep(5, in.dim - 1))
  
  mooFunction(
    name = "zdt4",
    id = sprintf("zdt4-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(zdt4, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


zdt4 = function(x, out.dim) {
  f1 = x[1L]
  m = length(x)
  g = 1 + 10 * (m - 1) + sum(x[-1L]^2 - 10 * cos(4 * pi * x[-1L]))
  f2 = g * (1 - sqrt(f1 / g))
  return(c(f1, f2))
}
