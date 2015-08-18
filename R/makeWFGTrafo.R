#' Make WFG Transformations
#'
#' @param arg [\code{list}] \cr
#'   List with \code{list(names, ids, params)}.
#' @param names [\code{character(1)}] \cr
#'   WFG trafo names.
#' @param ids [\code{vector}] \cr
#'   Vector ...
#' @param params [\code{list}] \cr
#'   List of parameters to this trafos.
#' @return A [\code{list}] of WFG trafos.
#' @export
makeWFGTrafo = function(arg) {
  
  funs = extractSubList(arg, "name")
  
  assertSubset(funs, c("identity", "b_poly", "b_flat", "b_param", "s_linear", 
    "s_decept", "s_multi", "r_sum", "r_nonseq"))
  
  params = extractSubList(arg, "params", simplify = FALSE)
  
  for (i in 1:length(arg)) {
    if (funs[i] == "identity") {
      if (!is.null(params[[i]]))
        stop("Identity trafo did not need parameters!")
    }
    if (funs[i] == "b_poly") {
      if (is.null(params[[i]]$alpha))
        stop("b_poly trafo need a alpha parameter!")
    }
    if (funs[i] %in% c("b_flat", "b_param", "s_decept", "s_multi")) {
      if (is.null(params[[i]]$A))
        stopf("%i trafo need a A parameter!", funs[i])
      if (is.null(params[[i]]$B))
        stopf("%i trafo need a B parameter!", funs[i])
      if (is.null(params[[i]]$C))
        stopf("%i trafo need a C parameter!", funs[i])
    }
    if (funs[i] %in% c("s_linear", "r_nonsep")) {
      if (is.null(params[[i]]$A))
        stopf("%i trafo need a A parameter!", funs[i])
    }
    if (funs[i] == "s_decept") {
      if (is.null(params[[i]]$y.prime))
        stop("s_decept trafo need a y.prime parameter!")
    }
    if (funs[i] == "r_sum") {
      if (is.null(params[[i]]$w))
        stop("r_sum trafo need a w parameter!")
    }
  }
  
  ids = extractSubList(arg, "ids", simplify = FALSE)
  
  funs = sapply(funs, function(fun) switch(fun,
    #identity = identity,
    identity = wfgTrafoIdentity,
    b_poly = wfgTrafoBPoly, 
    b_flat = wfgTrafoBFlat, 
    b_param = wfgTrafoBParam, 
    s_linear = wfgTrafoSLinear, 
    s_decept = wfgTrafoBSDecept, 
    s_multi = wfgTrafoSMulti, 
    r_sum = wfgTrafoRSum, 
    r_nonseq = wfgTrafoRNonsep))
  
  trafoFuns = lapply(seq_along(arg), function(i)
    do.call(funs[[i]], as.list(params[[i]])))
  
  trafoFun = function(x) {
    drop(unlist(sapply(seq_along(trafoFuns), function (i)
      do.call(trafoFuns[[i]], list(y = x[ids[[i]]])))))
  }
  
  trafoFun = addClasses(trafoFun, "wfgTrafoFun")
  return(trafoFun)
}

#arg = list(list(name = "identity", ids = 1:5), 
#   list(name = "b_poly", ids = 6:7, params = list(alpha = 0.001)),
#   list(name = "b_flat", ids = 8:11, params = list(A = 0.8, B = 0.75, C = 0.85)))
#f = makeWFGTrafo(arg)


