#' ZDT test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which ZDT function? Valid values are 1, 2, 3, 4, 6
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' Zitzler, Eckart ; Deb, Kalyanmoy ; Thiele, Lothar: Comparison of 
#' Multiobjective Evolutionary Algorithms: Empirical Results. In: Evolutionary
#' Computation 8 (2000), pp. 173-195
#' 
#' @export
#' 
#' @examples
#'  x = runif(30)
#'  fun = generateZDT(id = 1, in.dim = 30L, out.dim = 2L) 
#'  fun(x)
 
generateZDT = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("ZDT support only out.dim = 2.")
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("YOu set out.dim = %i and in.dim = %i, but in.dim must be greater than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:6)
  
  if (id == 4L)
    param.set = makeNumericParamSet(id = "x", len = in.dim, lower = c(0, rep(-5, in.dim - 1)), 
      upper = c(1, rep(5, in.dim - 1)))
  else
    param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  fun = switch(id,
    zdt1, 
    zdt2,
    zdt3,
    zdt4,
    ,
    zdt6
  )
  
  mooFunction(
    name = sprintf("zdt%i", id),
    id = sprintf("zdt%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = function(x) fun(x, out.dim = out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}


# definition of zdt1-6
zdt1 = function(x, out.dim) {
  f1 = x[1L]
  g = 1 + 9 * mean(x[-1L])
  f2 = g * (1 - sqrt(f1 / g))
  return(c(f1, f2))
}

zdt2 = function(x, out.dim) {
  f1 = x[1L]
  g = 1 + 9 * mean(x[-1L])
  f2 = g * (1 - (f1 / g)^2)
  return(c(f1, f2))
}

zdt3 = function(x, out.dim) {
  f1 = x[1L]
  g = 1 + 9 * mean(x[-1L])
  f2 = g * (1 - sqrt(f1 / g) - (f1 / g) * sin(10 * pi * f1))
  return(c(f1, f2))
}

zdt4 = function(x, out.dim) {
  f1 = x[1L]
  m = length(x)
  g = 1 + 10 * (m - 1) + sum(x[-1L]^2 - 10 * cos(4 * pi * x[-1L]))
  f2 = g * (1 - sqrt(f1 / g))
  return(c(f1, f2))
}

zdt6 = function(x, out.dim) {
  f1 = 1 - exp(-4 * x[1L]) * sin(6 * pi * x[1L])^6
  g = 1 + 9 * mean(x[-1L])^0.25
  f2 = g * (1 - (f1 / g)^2)
  return(c(f1, f2))
}