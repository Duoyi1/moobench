# MOP_C2 test function generator.

generateMOP_C2 = function(in.dim = 6L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = c(0, 0, 1, 0, 1, 0), 
      upper = c(10, 10, 5, 6, 5, 10)),
    forbidden = expression({ 
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      const1 = x[1L]  + x[2L] - 2 >= 0
      const2 = 6 - x[1L] - x[2L] >= 0
      const3 = 2 - x[2L] + x[1L] >= 0
      const4 = 2 - x[1L] + 3 * x[2L] >= 0
      const5 = 4 - (x[3L] - 3)^2  - x[4L] >= 0
      const6 = (x[5L] - 3)^2 + x[6L] - 4 >= 0
      
      all(c(const1, const2, const3, const4, const5, const6))
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "mop_c2",
    id = sprintf("mop_c2-%id-%id", in.dim, out.dim),
    fun = mop_c2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of mop_c2
mop_c2 = function(x) {
  f1 = -(25 * (x[1L] - 2)^2 + (x[2L] - 2)^2 + (x[3L] - 1)^2 + (x[4L] - 4)^2 + (x[5L] - 1)^2) 
  f2 = sum(x^2)
  return(c(f1, f2))
}