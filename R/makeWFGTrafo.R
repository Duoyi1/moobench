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
makeWFGTrafo = function(...) {

  funs = extractSubList(arg, "name")
  
  assertSubset(funs, c("identity", "b_poly", "b_flat", "b_param", "s_linear", 
    "s_decept", "s_multi", "r_sum", "r_nonseq"))
  
  params = extractSubList(arg, "params")
  
  for (i in 1:length(arg)) {
    if (funs[i] == "identity") {
      if (params[[i]] != is.null)
        stop("Identity trafo did not need parameters!")
    }
    if (funs[i] == "b_poly") {
      if (params[[i]]$alpha == is.null)
        stop("b_poly trafo need a alpha parameter!")
    }
    if (funs[i] %in% c("b_flat", "b_param", "s_decept", "s_multi")) {
      if (params[[i]]$A == is.null)
        stopf("%i trafo need a A parameter!", funs[i])
      if (params[[i]]$B == is.null)
        stopf("%i trafo need a B parameter!", funs[i])
      if (params[[i]]$C == is.null)
        stopf("%i trafo need a C parameter!", funs[i])
    }
    if (funs[i] %in% c("s_linear", "r_nonsep")) {
      if (params[[i]]$A== is.null)
        stopf("%i trafo need a A parameter!", funs[i])
    }
    if (funs[i] == "s_decept") {
      if (params[[i]]$y.prime == is.null)
        stop("s_decept trafo need a y.prime parameter!")
    }
    if (funs[i] == "r_sum") {
      if (params[[i]]$w == is.null)
        stop("r_sum trafo need a w parameter!")
    }
  }
  
  ids = extractSubList(arg, "ids")
  
  funs = sapply(funs, function(fun) switch(fun,
    identity = identity,
    b_poly = wfgTrafoBPoly, 
    b_flat = wfgTrafoBFlat, 
    b_param = wfgTrafoBParam, 
    s_linear = wfgTrafoSLinear, 
    s_decept = wfgTrafoBSDecept, 
    s_multi = wfgTrafoSMulti, 
    r_sum = wfgTrafoRSum, 
    r_nonseq = wfgTrafoRNonsep))
  
  
  lapply(seq_along(arg), function(i)
    rep(do.call(funs[[i]], params[i])), length(ids[[i]]))
  
  
}

# arg = list(list(name = "identity", ids = 1:5), 
#   list(name = "b_poly", ids = 6:11, params = list(alpha = 0.001)))




