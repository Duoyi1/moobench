# MOP5 test function generator.

generateMOP5 = function(in.dim = 2L, out.dim = 3L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -30, upper = 30)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mop5",
    id = sprintf("mop5-%id-%id", in.dim, out.dim),
    fun = mop5,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mop5
mop5 = function(x) {
  f1 = 0.5 * (x[1L]^2 + x[2L]^2) + sin(x[1L]^2 + x[2L]^2)
  f2 = (3 * x[1L] - 2 * x[2L] + 4)^2 / 8 + (x[1L] - x[2L] + 1)^2 / 27 + 15
  f3 = 1 / (x[1L]^2 + x[2L]^2 + 1) - 1.1 * exp(-x[1L]^2 - x[2L]^2)
  return(c(f1, f2, f3))
}