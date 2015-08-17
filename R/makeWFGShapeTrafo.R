makeWFGShapeTrafo = function(...){
  
  out.dim = out.dim
  
  if (length(arg) == 1)
    arg = rep(arg, out.dim)
  
  funs = extractSubList(arg, "name")
  
  assertSubset(funs, c("linear", "concave", "convex", "mixed", "disconnected"))
  
  params = extractSubList(arg, "params")
  
  for (i in 1:length(arg)) {
    if (funs[i] %in% c("linear", "concave", "convex")) {
      if (params[[i]] != is.null)
        stopf("%i shapes did not need parameters!", funs[i])
    }
    if (funs[i] == "mixed") {
      if (params[[i]]$alpha == is.null)
        stop("Mixed shapes need a alpha parameter!")
      if (params[[i]]$A == is.null)
        stop("Mixed shapes need a A parameter!")
    }
    if (funs[i] == "disconnected") {
      if (params[[i]]$alpha == is.null)
        stop("Disconnected shapes need a alpha parameter!")
      if (params[[i]]$beta == is.null)
        stop("Disconnected shapes need a beta parameter!")
      if (params[[i]]$A == is.null)
        stop("Disconnected shapes need a A parameter!")
    }
  }

  
  funs = sapply(funs, function(fun) switch(fun,
    linear = wfgShapeLinear,
    convex = wfgShapeConvex,
    concave = wfgShapeConcave,
    mixed = wfgShapeMixed,
    disconnected = wfgShapeDisconnected))
  
  lapply(seq_along(arg), function(i)
    do.call(funs[[i]], c(dim = i, out.dim = out.dim, params[i])))
  
}


# arg = list(list(name = "concave"), list(name = "concave"), 
#   list(name = "mixed", params = list(alpha = 1, A = 2L)))