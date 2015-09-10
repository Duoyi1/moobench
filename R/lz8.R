#' LZ8 test function generator.

generateLZ8 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  lz8 = function(x) {
    j = 2:length(x)
    j1 = j[j %% 2 == 1L]
    j2 = j[j %% 2 == 0L]
    
    y = function(j) {
      x[j] - x[1L]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
    }
    
    f1 = x[1L] + 2 / length(j1) * 
      (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / (sqrt(j1)))) + 2)
    f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
      (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / (sqrt(j2)))) + 2)
    return(c(f1, f2))
  }
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    
    des[, -1L] = t(sapply(x1, function(x) x^(0.5 * (1 + (3 * (j - 2)) / (in.dim - 2)))))

    des
  }
  
  mooFunction(
    name = "lz8",
    id = sprintf("lz8-%id-%id", in.dim, out.dim),
    fun = function(x) lz8(x),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
