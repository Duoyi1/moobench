#' UF4 test function generator.

generateUF4 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    
    des[, -1L] = t(sapply(x1, function(x) sin(6 * pi * x + (j * pi) / in.dim)))
    
    des
  }
  
  mooFunction(
    name = "uf4",
    id = sprintf("uf4-%id-%id", in.dim, out.dim),
    fun = uf4,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of uf4
uf4 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    sum(abs(y(j1)) / (1 + exp(2 * abs(y(j1)))))
  f2 = 1 - x[1L]^2 + 2 / length(j2) * 
    sum(abs(y(j2)) / (1 + exp(2 * abs(y(j2)))))
  return(c(f1, f2))
}
