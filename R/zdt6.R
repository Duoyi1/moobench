# ZDT6 test function generator.

generateZDT6 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    mat = matrix(0, nrow = n, ncol = in.dim - 1L)
    des[, -1L] = mat
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    f1 = runif(n, min = 0.2807753191, max = 1)
    f2 = 1 - f1^2
    
    des = cbind(f1, f2)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    as.data.frame(des)
  }
  
  mooFunction(
    name = "zdt6",
    id = sprintf("zdt6-%id-%id", in.dim, out.dim),
    fun = zdt6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of zdt6
zdt6 = function(x) {
  f1 = 1 - exp(-4 * x[1L]) * sin(6 * pi * x[1L])^6
  g = 1 + 9 * mean(x[-1L])^0.25
  f2 = g * (1 - (f1 / g)^2)
  return(c(f1, f2))
}
