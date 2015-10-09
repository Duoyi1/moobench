# MOP7 test function generator.

generateMOP7 = function(in.dim = 2L, out.dim = 3L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -30, upper = 30)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mop7",
    id = sprintf("mop7-%id-%id", in.dim, out.dim),
    fun = mop7,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mop7
mop7 = function(x) {
  f1 = (x[1L] - 2)^2 / 2 + (x[2L] + 1)^2 / 13 + 3
  f2 = (x[1L] + x[2L] - 3)^2 / 36 + (-x[1L] + x[2L] + 2)^2 / 8 - 17
  f3 = (x[1L] + 2 * x[2L] - 1)^2 / 175 + (-x[1L] + 2 * x[2L])^2 / 17 - 13
  return(c(f1, f2, f3))
}