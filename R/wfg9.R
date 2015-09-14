# WFG9 test function generator.

generateWFG9 = function(in.dim, out.dim, k) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 2 * 1:in.dim)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    for(i in (k + 1):(in.dim - 1))
      des[, i] = 2 * i * 0.35^(0.02 + 1.96 * rowMeans(des[, 1:(i - 1)]))^-1 
    
    des[, in.dim] = 2 * in.dim * 0.35
    
    des
  }
  
  mooFunction(
    name = "wfg9",
    id = sprintf("wfg9-%id-%id", in.dim, out.dim),
    fun = makeWfg9(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# definition of makewfg9
makeWfg9 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
    simplify = FALSE)))
  
  trafo1 = lapply(1:(n - 1), function(i)
    list(name = "b_param", ids = i, y.prime.ids = (i+1):in.dim, 
      params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)))
  trafo1 = makeWFGTrafo(c(trafo1, list(list(name = "identity", ids = n))))
  
  trafo2 = makeWFGTrafo(list(
    list(name = "s_decept", ids = 1:k, params = list(A = 0.35, B = 0.001, C = 0.05)),
    list(name = "s_multi", ids = (k + 1):n, params = list(A = 30, B = 95, C = 0.35))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_nonsep", ids = rIds[[i]], params = list(A = k / (out.dim - 1)))
  }),
    list(list(name = "r_nonsep", ids = (k + 1):n, params = list(A = n - k)))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}
