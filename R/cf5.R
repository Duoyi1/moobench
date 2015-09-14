# CF5 test function generator.

generateCF5 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      x[2L] - 0.8 * x[1L] * sin(6 * pi * x[1L] + 0.2 * pi) - 0.5 * x[1L] + 0.25 >= 0
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "cf5",
    id = sprintf("cf5-%id-%id", in.dim, out.dim),
    fun = cf5,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# definition of cf3-7
cf5 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y1 = x[j1] - 0.8 * x[1L] * cos(6 * pi * x[1L] + (j1 * pi) / length(x))
  y2 = x[j2] - 0.8 * x[1L] * sin(6 * pi * x[1L] + (j2 * pi) / length(x))
  
  f1 = x[1L] + sum(2 * y1^2 - cos(4 * pi * y1) + 1)
  if (y2[1] < ((3 / 2) * (1 - sqrt(2) / 2))) {
    f2 = 1 - x[1L] + sum(c(abs(y2[1L]), 2 * y2[-1L]^2 - cos(4 * pi * y2[-2L]) + 1))
  }
  else
    f2 = 1 - x[1L] + sum(c(0.125 + (y2[1L] - 1)^2, 2 * y2[-1L]^2 - cos(4 * pi * y2[-2L]) + 1))
  return(c(f1, f2))
}
