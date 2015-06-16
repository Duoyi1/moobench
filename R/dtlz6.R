#' DTLZ6 test function generator.
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
generateDTLZ6 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "dtlz6",
    id = sprintf("dtlz6-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(dtlz6, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


dtlz6 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1)]
  x.tail = x[out.dim:length(x)]
  
  g = 1 + (9 / length(x.tail)) * sum(x.tail)
  h = length(x) - sum((x.head / (1 + g)) * (1 + sin(3 * pi * x.head)))
  fm = (1 + g) * h
  
  c(x.head, fm)
}
