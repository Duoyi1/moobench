# WFG2 test function generator.

generateWFG2 = function(in.dim, out.dim, k) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 2 * 1:in.dim)
  
  paretoSet = function(n = out.dim * 100L) {
    # sample 10 times points and remove dominated ones - we have a disconnected front
    des = generateDesign(par.set = param.set, n = 10 * n)
  
    i = (k + 1):in.dim
    des[, (k + 1):in.dim] = matrix(2 * i * 0.35, nrow = nrow(des), ncol = in.dim - k,
      byrow = TRUE)
    
    des = des[!is_dominated(apply(des, 1, makeWfg2(in.dim, out.dim, k))), ]
    des = des[sample(nrow(des), n), ]
    
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    x = matrix(runif(10 * n * (out.dim - 1)), nrow = 10 * n, ncol = out.dim - 1) 
    
    shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim - 1L, list(name = "convex"), simplify = FALSE), 
      list(list(name = "disconnected", params = list(alpha = 1, beta = 1, A = 5L)))))
    
    des = sapply(seq_along(shapeTrafos), function(i) 2 * i * apply(x, 1, shapeTrafos[[i]]))
    
    des = des[!is_dominated(t(des)), ]
    des = des[sample(nrow(des), n), ]
    
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    as.data.frame(des)
  }
  
  mooFunction(
    name = "wfg2",
    id = sprintf("wfg2-%id-%id", in.dim, out.dim),
    fun = makeWfg2(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# definition of makewfg2
makeWfg2 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  l = n - k
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim - 1L, list(name = "convex"), simplify = FALSE), 
    list(list(name = "disconnected", params = list(alpha = 1, beta = 1, A = 5L)))))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  
  rIds1 = split((k + 1):in.dim, rep(1:(l / 2), each = 2L))
  #lapply((k + 1):(k + l/2), function(i) (k + 2 * (i - k) - 1):(k + 2 * (i - k)))
  trafo2 = makeWFGTrafo(c(list(
    list(name = "identity", ids = 1:k)),
    lapply(1:(l / 2), function(i) {
      list(name = "r_nonsep", ids = rIds1[[i]], params = list(A = 2))
    })
  ))
  
  rIds2  = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds2[[i]], params = list(w = rep(1, length(rIds2[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):(k + l / 2), params = list(w = rep(1, l / 2))))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}
