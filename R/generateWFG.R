#' WFG test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which WFG function? Valid values are 1, 2, ..., 9
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @param k [\code{integer(1)}] \cr
#'   Number position-related parameters.
#' @return A \code{mooFunction}.
#' 
#' @export

generateWFG = function(id, in.dim, out.dim, k) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  k = asCount(k)
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greater than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:9)
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  f = switch(id,
    makeWfg1(in.dim, out.dim = out.dim, k = k), 
    makeWfg2(in.dim, out.dim = out.dim, k = k), 
    makeWfg3(in.dim, out.dim = out.dim, k = k), 
    makeWfg4(in.dim, out.dim = out.dim, k = k), 
    makeWfg5(in.dim, out.dim = out.dim, k = k), 
    makeWfg6(in.dim, out.dim = out.dim, k = k), 
    makeWfg7(in.dim, out.dim = out.dim, k = k), 
    makeWfg8(in.dim, out.dim = out.dim, k = k), 
    makeWfg9(in.dim, out.dim = out.dim, k = k)
  )
  
  mooFunction(
    name = sprintf("wfg%i", id),
    id = sprintf("wfg%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = f,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}

# definition of makewfg1-9
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
  
  rIds = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
    #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo4 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = 2 * rIds[[i]]))
    }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = 2 * (k + 1):n)))
    ))
  
  trafos = list(trafo1, trafo2, trafo3, trafo4)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}

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
      list(name = "r_nonseq", ids = rIds1[[i]], params = list(A = 2))
    })
  ))
  
  rIds2  = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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
      list(name = "r_nonseq", ids = rIds1[[i]], params = list(A = 2))
    })
  ))
  
  rIds2  = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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

  rIds = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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

  rIds = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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
  
  rIds = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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
  
  rIds = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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

  rIds = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
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
  
  rIds2 = split(1:k, rep(1:(k / (out.dim - 1)), each = out.dim - 1))
    #lapply(1:(out.dim - 1), function(i) ((i - 1) * k / (out.dim -1) + 1):(i * k / (out.dim - 1)))
  trafo3 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_nonsep", ids = rIds2[[i]], params = list(A = k / (out.dim - 1)))
  }),
    list(list(name = "r_nonsep", ids = (k + 1):n, params = list(A = n - k)))
  ))
  
  trafos = list(trafo1, trafo2, trafo3)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}