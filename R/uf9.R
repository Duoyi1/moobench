#' UF9 test function generator.
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
generateUF9 = function(in.dim = 30, out.dim = 3) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 3L)
    stop("UF9 support only out.dim = 3.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf9",
    id = sprintf("uf9-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(uf9, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}

uf9 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1]
  j2 = j[j %% 3 == 2]
  j3 = j[j %% 3 == 0]
  
  f1 = 0.5 * (max(0, 1.1 * (1 - 4 * (2 * x[1L] - 1)^2)) + 2 * x[1L]) * x[2L] + 
    2 / length(j1) * sum((x[j1] - 2 * x[2] * sin(2 * pi * x[1] + (j1 * pi) / length(x)))^2)
  f2 = 0.5 * (max(0, 1.1 * (1 - 4 * (2 * x[1L] - 1)^2)) - 2 * x[1L] + 2) * x[2L] +
    2 / length(j2) * 
    sum((x[j2] - 2 * x[2L] * sin(2 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  f3 = 1 - x[2] + 2 / length(j3) * 
    sum((x[j3] - 2 * x[2L] * sin(2 * pi * x[1L] + (j3 * pi) / length(x)))^2)
  
  return(c(f1, f2, f3))
}
