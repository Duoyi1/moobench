# UF10 test function generator.

generateUF10 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    x2 = des[, 2L]
    j = 3:in.dim
    
    des[, -(1:2)] = t(sapply(seq_along(x1), function(i) 
      2 * x2[i] * sin(2 * pi * x1[i] + (j * pi) / in.dim)))
    
    des
  }
  
  mooFunction(
    name = "uf10",
    id = sprintf("uf10-%id-%id", in.dim, out.dim),
    fun = uf10,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of uf10
uf10 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1L]
  j2 = j[j %% 3 == 2L]
  j3 = j[j %% 3 == 0L]
  
  y = function(j) {
    x[j] - 2 * x[2L] * sin(2 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = cos(0.5 * x[1L] * pi) * cos(0.5 * x[2L] * pi) + 
    2 / length(j1) * sum(4 * y(j1)^2 - cos(8 * pi * y(j1)) + 1)
  f2 = cos(0.5 * x[1L] * pi) * sin(0.5 * x[2L] * pi) + 
    2 / length(j2) * sum(4 * y(j2)^2 - cos(8 * pi * y(j2)) + 1)
  f3 = sin(0.5 * x[1L] * pi) + 
    2 / length(j3) * sum(4 * y(j3)^2 - cos(8 * pi * y(j3)) + 1)
  
  return(c(f1, f2, f3))
}
