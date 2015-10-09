# UF9 test function generator.

generateUF9 = function(in.dim = 30L, out.dim = 2L) {
  
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
  
  paretoFront = function(n = out.dim * 100L) {
    if (n %% 2 == 1) {
      f3 = runif(floor(n / 2))
      f1 = runif(floor(n / 2), min = 0, max = 0.25 * (1 - f3))
      f2 = 1 - f1 - f3
      
      ff3 = runif(ceiling(n / 2))
      ff1 = runif(ceiling(n / 2), min = 0.75 * (1 - ff3), max = 1)
      ff2 = 1 - ff1 - ff3
    } else {
      pts1 = runif(n = n / 2, min = 0.25, max = 0.5)
      pts2 = runif(n = n / 2, min = 0.75, max = 1)
      f3 = runif(n / 2)
      f1 = runif(n / 2, min = 0, max = 0.25 * (1 - f3))
      f2 = 1 - f1 - f3
      
      ff3 = runif(n / 2)
      ff1 = runif(n / 2, min = 0.75 * (1 - ff3), max = 1)
      ff2 = 1 - ff1 - ff3
    }
    
    des = cbind(c(f1, ff1), c(f2, ff2), c(f3, ff3))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  mooFunction(
    name = "uf9",
    id = sprintf("9-%id-%id", in.dim, out.dim),
    fun = uf9,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of uf9
uf9 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1L]
  j2 = j[j %% 3 == 2L]
  j3 = j[j %% 3 == 0L]
  
  f1 = 0.5 * (max(0, 1.1 * (1 - 4 * (2 * x[1L] - 1)^2)) + 2 * x[1L]) * x[2L] + 
    2 / length(j1) * sum((x[j1] - 2 * x[2] * sin(2 * pi * x[1] + (j1 * pi) / length(x)))^2)
  f2 = 0.5 * (max(0, 1.1 * (1 - 4 * (2 * x[1L] - 1)^2)) - 2 * x[1L] + 2) * x[2L] +
    2 / length(j2) * 
    sum((x[j2] - 2 * x[2L] * sin(2 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  f3 = 1 - x[2] + 2 / length(j3) * 
    sum((x[j3] - 2 * x[2L] * sin(2 * pi * x[1L] + (j3 * pi) / length(x)))^2)
  
  return(c(f1, f2, f3))
}