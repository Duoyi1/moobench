#' UF10 test function generator.
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
generateUF10 = function(in.dim = 30, out.dim = 3) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 3L)
    stop("UF10 support only out.dim = 3.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf10",
    id = sprintf("uf10-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(uf10, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}

uf10 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 0]
  j2 = j[j %% 3 == 2]
  j3 = j[j %% 3 == 1]
  
  y = function(j) {
    x[j] - 2 * x[2] * sin(2 * pi * x[1] + (j * pi) / length(x))
  }
  
  f1 = cos(0.5 * x[1] * pi) * cos(0.5 * x[2] * pi) + 
    2 / length(j1) * sum(4 * y(j1)^2 - cos(8 * pi * y(j1)) + 1)
  f2 = cos(0.5 * x[1] * pi) * sin(0.5 * x[2] * pi) + 
    2 / length(j2) * sum(4 * y(j2)^2 - cos(8 * pi * y(j2)) + 1)
  f3 = sin(0.5 * x[1] * pi) + 
    2 / length(j3) * sum(4 * y(j3)^2 - cos(8 * pi * y(j3)) + 1)
  
  return(c(f1, f2, f3))
}
