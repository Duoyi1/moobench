# WFG1 test function generator.

generateWFG1 = function(in.dim, out.dim, k) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 2 * 1:in.dim)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    i = (k + 1):in.dim
    des[, (k + 1):in.dim] = matrix(2 * i * 0.35, nrow = nrow(des), ncol = in.dim - k,
      byrow = TRUE)
    des
  }
  
  mooFunction(
    name = "wfg1",
    id = sprintf("wfg1-%id-%id", in.dim, out.dim),
    fun = makeWfg1(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# definition of makewfg1
makeWfg1 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim - 1L, list(name = "convex"), simplify = FALSE), 
    list(list(name = "mixed", params = list(alpha = 1, A = 5L)))))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  trafo2 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k), 
    list(name = "b_flat", ids = (k + 1):n, params = list(A = 0.8, B = 0.75, C = 0.85))
  ))
  trafo3 = makeWFGTrafo(list(
    list(name = "b_poly", ids = 1:n, params = list(alpha = 0.02))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  trafo4 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = 2 * rIds[[i]]))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = 2 * (k + 1):n)))
  ))
  
  trafos = list(trafo1, trafo2, trafo3, trafo4)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}
