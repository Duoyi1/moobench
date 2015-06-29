#' LZ7 test function generator.
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
generateLZ7 = function(in.dim = 30, out.dim = 2) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("LZ7 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "lz7",
    id = sprintf("lz7-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz7, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


lz7 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1]
  j2 = j[j %% 2 == 0]
  
  y = function(j) {
    x[j] - x[1]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
  }
  
  f1 = x[1] + 2 / length(j1) * 
    sum(y(j1)^2 - cos(8 * y(j1) * pi) + 1)
  f2 = 1 - sqrt(x[1]) + 2 / length(j2) * 
    sum(4 * y(j2)^2 - cos(8 * y(j2) * pi) + 1)
  return(c(f1, f2))
}
