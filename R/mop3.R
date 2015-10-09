# MOP3 test function generator.

generateMOP3 = function(in.dim = 2L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -pi, upper = pi)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mop3",
    id = sprintf("mop3-%id-%id", in.dim, out.dim),
    fun = mop3,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mop3
mop3 = function(x) {
  A1 = 0.5 * sin(1) - 2 * cos(1) + sin(2) - 1.5 * cos(2)
  A2 = 1.5 * sin(1) - cos(1) + 2 * sin(2) - 0.5 * cos(2)
  B1 = 0.5 * sin(x[1L]) - 2 * cos(x[1L]) + sin(x[2L]) - 1.5 * cos(x[2L])
  B2 = 1.5 * sin(x[1L]) - cos(x[1L]) + 2 * sin(x[2L]) - 0.5 * cos(x[2L])
  f1 = -1 - (A1 - B1)^2 - (A2 - B2)^2
  f2 = -(x[1L] + 3)^2 - (x[2L] + 1)^2
  return(c(f1, f2))
}