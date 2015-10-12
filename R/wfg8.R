# WFG8 test function generator.

generateWFG8 = function(in.dim, out.dim, k) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 2 * 1:in.dim)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    for(i in (k + 1):in.dim)
      des[, i] = 2 * i * 0.35^(0.02 + 49.98 * ((0.98 / 49.98) - (1 - 2 * rowMeans(des[, 1:(i - 1), drop = FALSE])) * 
          abs(floor(0.5 - rowMeans(des[, 1:(i - 1), drop = FALSE])) + (0.98 / 49.98))))^-1
    
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    x = matrix(runif(n * (out.dim - 1)), nrow = n, ncol = out.dim - 1) 
    
    shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
      simplify = FALSE)))
    
    des = sapply(seq_along(shapeTrafos), function(i) 2 * i * apply(x, 1, shapeTrafos[[i]]))
    
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  mooFunction(
    name = "wfg8",
    id = sprintf("wfg8-%id-%id", in.dim, out.dim),
    fun = makeWfg8(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# definition of makewfg8
makeWfg8 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
    simplify = FALSE)))
  
  trafo1 = lapply((k + 1):n, function(i)
    list(name = "b_param", ids = i, y.prime.ids = 1:(i - 1), 
      params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)))
  trafo1 = makeWFGTrafo(c(list(list(name = "identity", ids = 1:k)), trafo1))
  
  trafo2 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = rep(1, length(rIds[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = rep(1, n - k))))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}
