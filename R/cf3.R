#' CF3 test function generator.

generateCF3 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = cf3(x)
      f[2L] + f[1L]^2 - sin(2 * pi * (f[1L]^2 - f[2L] + 1)) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "cf3",
    id = sprintf("cf3-%id-%id", in.dim, out.dim),
    fun = cf3,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definition of cf3
cf3 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - x[1L]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / (sqrt(j1)))) + 2)
  f2 = 1 - x[1L]^2 + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / (sqrt(j2)))) + 2)
  return(c(f1, f2))
}
