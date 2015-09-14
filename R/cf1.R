# CF1 test function generator.

generateCF1 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = lz1(x)
      f[1L] + f[2L] - abs(sin(10 * pi * (f[1L] - f[2L] + 1))) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "cf1",
    id = sprintf("cf1-%id-%id", in.dim, out.dim),
    fun = lz1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
