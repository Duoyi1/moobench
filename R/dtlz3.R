#' DTLZ3 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references DTLZ Reference Einfuegen
#' 
#' @export
#' 
generateDTLZ3 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "dtlz3",
    id = sprintf("dtlz3-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(dtlz3, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


dtlz3 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1)] * 0.5 * pi
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev((1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))) * 
      c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
}
