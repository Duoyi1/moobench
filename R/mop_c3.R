# MOP_C3 test function generator.

generateMOP_C3 = function(in.dim = 6L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = -4, upper = 4),
    forbidden = expression({ 
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      const1 = - 4 * x[1L] + 4 - x[2L] > 0
      const2 = x[1L] + 1 > 0
      const3 = x[2L] - x[1L] + 2 > 0
      
      all(c(const1, const2, const3))
    }))
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mop_c3",
    id = sprintf("mop_c3-%id-%id", in.dim, out.dim),
    fun = mop_c3,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mop_c3
mop_c3 = function(x) {
  f1 = (x[1L] - 2)^2 / 2 + (x[2L] + 1)^2 / 13 + 3
  f2 = (x[1L] + x[2L] - 3)^2 / 175 + (2 * x[2L] - x[1L])^2 / 17 - 13
  f3 = (3 * x[1L] - 2 * x[2L] + 4)^2 / 8 + (x[1L] - x[2L] + 1)^2 / 27 + 15
  return(c(f1, f2, f3))
}