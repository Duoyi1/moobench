# WFG3 test function generator.

generateWFG3 = function(in.dim, out.dim, k) {
  
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
    name = "wfg3",
    id = sprintf("wfg3-%id-%id", in.dim, out.dim),
    fun = makeWfg3(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# definition of makewfg3
makeWfg3 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = c(1, rep(0, out.dim - 2))
  n = in.dim
  x.max = 2 * 1:n
  
  l = n - k
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "linear"), 
    simplify = FALSE)))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  
  rIds1 = split((k + 1):in.dim, rep(1:(l / 2), each = 2L))
  trafo2 = makeWFGTrafo(c(list(
    list(name = "identity", ids = 1:k)),
    lapply(1:(l/2), function(i) {
      list(name = "r_nonsep", ids = rIds1[[i]], params = list(A = 2))
    })
  ))
  
  rIds2  = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds2[[i]], params = list(w = rep(1, length(rIds2[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):(k + l / 2), params = list(w = rep(1, l / 2))))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}
