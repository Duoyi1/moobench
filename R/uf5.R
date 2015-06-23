#' UF5 test function generator.
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
generateUF5 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("UF5 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf5",
    id = sprintf("uf5-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(uf5, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


uf5 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 0]
  j2 = j[j %% 2 == 1]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1] + (j * pi) / length(x))
  }
  
  f1 = x[1] + 0.15 * abs(sin(20 * pi * x[1])) + 2 / length(j1) * 
    sum(2 * y(j1)^2 - cos(4 * pi * y(j1)) + 1)
  f2 = 1 - x[1] + 0.15 * abs(sin(20 * pi * x[1])) + 2 / length(j2) * 
    sum(2 * y(j2)^2 - cos(4 * pi * y(j2)) + 1)
  return(c(f1, f2))
}