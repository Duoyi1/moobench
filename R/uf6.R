#' UF6 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references Cec 09 Reference Einfuegen
#' 
#' @export
#' 
generateUF6 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("UF6 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf6",
    id = sprintf("uf6-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(uf6, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


uf6 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 0]
  j2 = j[j %% 2 == 1]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1] + (j * pi) / length(x))
  }
  
  f1 = x[1] + max(0, 0.7 * sin(4 * pi * x[1])) + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / sqrt(j1))) + 2)
  f2 = 1 - x[1] + max(0, 0.7 * sin(4 * pi * x[1])) + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / sqrt(j2))) + 2)
  return(c(f1, f2))
}