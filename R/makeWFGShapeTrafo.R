#' Make WFG Shapes
#'
#' @param arg [\code{list}] \cr
#'   List with \code{list(names, params)}.
#' @param names [\code{character(1)}] \cr
#'   WFG shape names.
#' @param params [\code{list}] \cr
#'   List of parameters to this shape.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A [\code{list}] of WFG shapes.
#' @export
makeWFGShapeTrafo = function(arg, out.dim){
  
  out.dim = out.dim
  
  if (length(arg) == 1)
    arg = rep(arg, out.dim)
  
  funs = extractSubList(arg, "name")
  
  assertSubset(funs, c("linear", "concave", "convex", "mixed", "disconnected"))
  
  params = extractSubList(arg, "params")
  
  for (i in 1:length(arg)) {
    if (funs[i] %in% c("linear", "concave", "convex")) {
      if (!is.null(params[[i]]))
        stopf("%i shapes did not need parameters!", funs[i])
    }
    if (funs[i] == "mixed") {
      if (is.null(params[[i]]$alpha))
        stop("Mixed shapes need a alpha parameter!")
      if (is.null(params[[i]]$A))
        stop("Mixed shapes need a A parameter!")
    }
    if (funs[i] == "disconnected") {
      if (is.null(params[[i]]$alpha))
        stop("Disconnected shapes need a alpha parameter!")
      if (is.null(params[[i]]$beta))
        stop("Disconnected shapes need a beta parameter!")
      if (is.null(params[[i]]$A))
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
    do.call(funs[[i]], insert(unlist(params[i]), list(dim = i, out.dim = out.dim))))
}


#arg = list(list(name = "concave"), list(name = "concave"), 
#   list(name = "mixed", params = list(alpha = 1, A = 2L)))
#makeWFGShapeTrafo(arg, out.dim = 3)
