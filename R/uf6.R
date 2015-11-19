# UF6 test function generator.

generateUF6 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim,
    lower = c(0, rep(-1, in.dim - 1)), upper = 1)
  
  paretoSet = NULL
  
  paretoFront = function(n = out.dim * 100L) {
    if (n %% 2 == 1) {
      pts1 = runif(n = floor(n / 2), min = 0.25, max = 0.5)
      pts2 = runif(n = floor(n / 2), min = 0.75, max = 1)
    } else {
      pts1 = runif(n = n / 2, min = 0.25, max = 0.5)
      pts2 = runif(n = n / 2 - 1, min = 0.75, max = 1)
    }
    
    des = cbind(c(0, pts1, pts2), 1 - c(0, pts1, pts2))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    as.data.frame(des)
  }
  
  mooFunction(
    name = "uf6",
    id = sprintf("uf6-%id-%id", in.dim, out.dim),
    fun = uf6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of uf6
uf6 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + max(0, 0.7 * sin(4 * pi * x[1L])) + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / sqrt(j1))) + 2)
  f2 = 1 - x[1L] + max(0, 0.7 * sin(4 * pi * x[1L])) + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / sqrt(j2))) + 2)
  return(c(f1, f2))
}
