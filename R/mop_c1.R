# MOP_C1 test function generator.

generateMOP_C1 = function(in.dim = 2L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = c(0, 0), upper = c(5, 3)),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      const1 = (x[1L] - 5)^2 + (x[2L])^2 - 25 <= 0
      const2 = -(x[1L] - 8)^2 - (x[2L] + 3)^2 + 7.7 <= 0
      
      const1 && const2
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "mop_c1",
    id = sprintf("mop_c1-%id-%id", in.dim, out.dim),
    fun = mop_c1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of mop_c1
mop_c1 = function(x) {
  f1 = 4 * x[1L]^2 + 4 * x[2L]^2
  f2 = (x[1L] - 5)^2 + (x[2L] - 5)^2
  return(c(f1, f2))
}