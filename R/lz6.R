#' LZ6 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references LZ Reference Einfuegen
#' 
#' @export
#' 
generateLZ6 = function(in.dim = 30, out.dim = 3) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 3L)
    stop("LZ6 support only out.dim = 3.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "lz6",
    id = sprintf("lz6-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz6, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


lz6 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[(j - 2) %% 3 == TRUE]
  j2 = j[(j - 1) %% 3 == TRUE]
  j3 = j[j %% 3 == TRUE]
  
  f1 = cos(0.5 * x[1] * pi) * cos(0.5 * x[2] * pi) + 2 / length(j1) * 
    sum((x[j1] - 2 * x[2] * sin(2 * pi * x[1] + (j1 * pi) / length(x)))^2)
  f2 = cos(0.5 * x[1] * pi) * sin(0.5 * x[2] * pi) + 2 / length(j2) * 
    sum((x[j2] - 2 * x[2] * sin(2 * pi * x[1] + (j2 * pi) / length(x)))^2)
  f3 = sin(0.5 * x[1] * pi) + 2 / length(j3) * 
    sum((x[j3] - 2 * x[2] * sin(2 * pi * x[1] + (j3 * pi) / length(x)))^2)
  
  return(c(f1, f2, f3))
}
