#' LZ5 test function generator.

generateLZ5 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  lz5 = function(x) {
    j = 2:length(x)
    j1 = j[j %% 2 == 1L]
    j2 = j[j %% 2 == 0L]
    
    f1 = x[1L] + 2 / length(j1) * 
      sum((x[j1] - (0.3 * x[1L]^2 * cos(24 * pi * x[1L] + (4 * j1 * pi) / length(x)) + 0.6 * x[1L]) * 
          cos(6 * pi * x[1L] + (j1 * pi) / length(x) ))^2)
    f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
      sum((x[j2] - (0.3 * x[1L]^2 * cos(24 * pi * x[1L] + (4 * j2 * pi) / length(x)) + 0.6 * x[1L]) * 
          sin(6 * pi * x[1L] + (j2 * pi) / length(x) ))^2)
    return(c(f1, f2))
  }
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    j1 = j[j %% 2 == 1L]
    j2 = j[j %% 2 == 0L]
    
    des[, j1] = t(sapply(x1, function(x) 0.3 * x^2 * cos(24 * pi * x + (4 * j1 * pi) / 
        in.dim + 0.6 * x) * cos(6 * pi * x + (j1 * pi) / in.dim)))
    des[, j2] = t(sapply(x1, function(x) 0.3 * x^2 * cos(24 * pi * x + (4 * j2 * pi) / 
        in.dim + 0.6 * x) * sin(6 * pi * x + (j2 * pi) / in.dim)))

    des
  }
  
  mooFunction(
    name = "lz5",
    id = sprintf("lz5-%id-%id", in.dim, out.dim),
    fun = lz5,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
