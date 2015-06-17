#' LZ5 test function generator.
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
generateLZ5 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("LZ5 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "lz5",
    id = sprintf("lz5-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz5, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


lz5 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == TRUE]
  j2 = j[j %% 2 == FALSE]
  
  f1 = x[1] + 2 / length(j1) * 
    sum((x[j1] - (0.3 * x[1]^2 * cos(24 * pi * x[1] + (4 * j1 * pi) / length(x) + 0.6 * x[1])) * 
        cos(6 * pi * x[1] + (j1 * pi) / length(x) ))^2)
  f2 = 1 - sqrt(x[1]) + 2 / length(j2) * 
    sum((x[j1] - (0.3 * x[1]^2 * cos(24 * pi * x[1] + (4 * j1 * pi) / length(x) + 0.6 * x[1])) * 
        sin(6 * pi * x[1] + (j1 * pi) / length(x) ))^2)
  return(c(f1, f2))
}