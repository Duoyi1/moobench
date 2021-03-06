# DTLZ6 test function generator.

generateDTLZ6 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = 10 * n)
    des[, out.dim:in.dim] = 0
    
    des = des[!is_dominated(apply(des, 1, dtlz6, out.dim = out.dim)), ]
    des = des[sample(nrow(des), n), ]
    
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    ps = paretoSet(n)
    des = t(apply(ps, 1, dtlz6, out.dim = out.dim))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    as.data.frame(des)
  }
  
  mooFunction(
    name = "dtlz6",
    id = sprintf("dtlz6-%id-%id", in.dim, out.dim),
    fun = function(x) dtlz6(x, out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of dtlz6
dtlz6 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]
  x.tail = x[out.dim:length(x)]
  
  g = 1 + (9 / length(x.tail)) * sum(x.tail)
  h = length(x) - sum((x.head / (1 + g)) * (1 + sin(3 * pi * x.head)))
  fm = (1 + g) * h
  
  c(x.head, fm)
}