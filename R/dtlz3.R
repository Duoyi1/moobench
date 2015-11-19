# DTLZ3 test function generator.

generateDTLZ3 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des[, out.dim:in.dim] = 0.5
    
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    des = matrix(runif(out.dim * n), nrow = n, ncol = out.dim)
    des = des / rowSums(des)
    des = sqrt(des)
    
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    as.data.frame(des)
  }
  
  mooFunction(
    name = "dtlz3",
    id = sprintf("dtlz3-%id-%id", in.dim, out.dim),
    fun = function(x) dtlz3(x, out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of dtlz3
dtlz3 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)] * 0.5 * pi
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev((1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))) * 
      c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
}