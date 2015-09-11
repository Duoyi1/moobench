#' WFG2 test function generator.

generateWFG2 = function(in.dim, out.dim, k) {
  
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
    name = "wfg2",
    id = sprintf("wfg2-%id-%id", in.dim, out.dim),
    fun = makeWfg2(in.dim, out.dim, k),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
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
  #lapply((k + 1):(k + l/2), function(i) (k + 2 * (i - k) - 1):(k + 2 * (i - k)))
  trafo2 = makeWFGTrafo(c(list(
    list(name = "identity", ids = 1:k)),
    lapply(1:(l/2), function(i) {
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

makeWfg4 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
    simplify = FALSE)))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "s_multi", ids = 1:n, params = list(A = 30, B = 10, C = 0.35))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo2 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = rep(1, length(rIds[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = rep(1, n - k))))
  ))
  
  trafos = list(trafo1, trafo2)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}

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
  #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo2 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = rep(1, length(rIds[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = rep(1, n - k))))
  ))
  
  trafos = list(trafo1, trafo2)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}

makeWfg6 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
    simplify = FALSE)))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo2 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_nonsep", ids = rIds[[i]], params = list(A = k / (out.dim - 1)))
  }),
    list(list(name = "r_nonsep", ids = (k + 1):n, params = list(A = n - k)))
  ))
  
  trafos = list(trafo1, trafo2)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}

makeWfg7 = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim, list(name = "concave"), 
    simplify = FALSE)))
  
  
  trafo1 = lapply(1:k, function(i)
    list(name = "b_param", ids = i, y.prime.ids = (i+1):in.dim, 
      params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)))
  trafo1 = makeWFGTrafo(c(trafo1, list(list(name = "identity", ids = (k + 1):n))))
  
  trafo2 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))
  #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = rep(1, length(rIds[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = rep(1, n - k))))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}

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
  #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = rep(1, length(rIds[[i]]))))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = rep(1, n - k))))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}

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
