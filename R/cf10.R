#' CF10 test function generator.

generateCF10= function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = uf10(x)
      (f[1L]^2 + f[2L]^2) / (1 - f[3L]^2) - 
        sin(2 * pi * ((f[1L]^2 - f[2L]^2) / (1 - f[3L]^2) + 1)) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "cf10",
    id = sprintf("cf10-%id-%id", in.dim, out.dim),
    fun = uf10,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

