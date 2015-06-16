#' DTLZ1 test function generator.
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
generateDTLZ11 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "dtlz1",
    id = sprintf("dtlz1-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(dtlz1, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


dtlz1 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev(0.5 * c(1, cumprod(x.head)) * c(1 - x.head, 1) * 
      (1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))))
}


