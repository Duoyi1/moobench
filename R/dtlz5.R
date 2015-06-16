#' DTLZ5 test function generator.
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
generateDTLZ5 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "dtlz5",
    id = sprintf("dtlz5-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(dtlz5, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


dtlz5 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  g = sum(x.tail)^0.1
  
  theta = numeric(out.dim - 1)
  theta[1] = x.head[1] * 0.5 * pi
  theta[2:(out.dim - 1)] = (pi / (4 * (1 + g))) * (1 + 2 * g * x.head[2:(out.dim - 1)]) * 0.5 * pi
  
  rev((1 + g) * c(sin(theta), 1) * c(1, cumprod(cos(theta))))
}
