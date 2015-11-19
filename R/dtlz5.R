# DTLZ5 test function generator.

generateDTLZ5 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des[, out.dim:in.dim] = 0.5
    
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    ps = paretoSet(n)
    des = t(apply(ps, 1, dtlz5, out.dim = out.dim))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    as.data.frame(des)
  }
  
  mooFunction(
    name = "dtlz5",
    id = sprintf("dtlz5-%id-%id", in.dim, out.dim),
    fun = function(x) dtlz5(x, out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of dtlz5
dtlz5 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]
  x.tail = x[out.dim:length(x)]
  
  g = sum(x.tail^0.1)
  theta = numeric(out.dim - 1L)
  if (out.dim > 1L) 
    theta[1L] = x.head[1L] * 0.5 * pi
  if (out.dim == 3L) 
    theta[2L] = (pi / (4 * (1 + g))) * (1 + 2 * g * x.head[2L]) * 0.5 * pi
  if (out.dim > 3L)
    theta[2:(out.dim - 1L)] = (pi / (4 * (1 + g))) * (1 + 2 * g * x.head[2:(out.dim - 1L)]) * 0.5 * pi
  
  rev((1 + g) * c(sin(theta), 1) * c(1, cumprod(cos(theta))))
}
