# UF5 test function generator.

generateUF5 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim,
    lower = c(0, rep(-1, in.dim - 1)), upper = 1)
  
  paretoSet = NULL
  
  paretoFront = function(n) {
    message("UF5 has discret Pareto front. Ignoring n and return all Pareto Optimal points.")
    cbind((0:20) / 20, 1 - (0:20) / 20)
  }
  
  mooFunction(
    name = "uf5",
    id = sprintf("uf5-%id-%id", in.dim, out.dim),
    fun = uf5,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of uf5
uf5 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + 0.15 * abs(sin(20 * pi * x[1L])) + 2 / length(j1) * 
    sum(2 * y(j1)^2 - cos(4 * pi * y(j1)) + 1)
  f2 = 1 - x[1L] + 0.15 * abs(sin(20 * pi * x[1L])) + 2 / length(j2) * 
    sum(2 * y(j2)^2 - cos(4 * pi * y(j2)) + 1)
  return(c(f1, f2))
}
