# WFG5 test function generator.

generateWFG5 = function(in.dim, out.dim, k) {
  
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
  
  paretoFront = NULL
  
  mooFunction(
    name = "wfg5",
    id = sprintf("wfg5-%id-%id", in.dim, out.dim),
    fun = makeWfg5(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# definition of makewfg5
makeWfg5 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
    simplify = FALSE)))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "s_decept", ids = 1:n, params = list(A = 0.35, B = 0.001, C = 0.05))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  trafo2 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = rep(1, length(rIds[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = rep(1, n - k))))
  ))
  
  trafos = list(trafo1, trafo2)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}
