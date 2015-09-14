#' CF7 test function generator.

generateCF7 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = lz6(x)
      (f[1L]^2 + f[2L]^2) / (1 - f[3L]^2) - 
        4 * abs(sin(2 * pi * ((f[1L]^2 - f[2L]^2) / (1 - f[3L]^2) + 1))) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "cf7",
    id = sprintf("cf7-%id-%id", in.dim, out.dim),
    fun = cf7,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definition of cf7
cf7 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y1 = x[j1] - cos(6 * pi * x[1L] + (j1 * pi) / length(x))
  y2 = x[j2] - sin(6 * pi * x[1L] + (j2 * pi) / length(x))
  
  f1 = x[1L] + sum(2 * y1^2 - cos(4 * pi * y1) + 1)
  f2 = (1 - x[1L])^2 + sum(c(y2[1:2]^2, 2 * y2[-(1:2)]^2 - cos(4 * pi * y2[-(1:2)]) + 1))
  return(c(f1, f2))
}
