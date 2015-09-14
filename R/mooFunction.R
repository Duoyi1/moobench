#' Multi Objective Optimization Function
#' 
#' Define a new \code{mooFunction} object.
#'
#' @param name [\code{character(1)}] \cr
#'   Name of function.
#' @param id [\code{character(1)}] \cr
#' Short id for the function. Must be unique to the
#'   function instance and should not contain any other characters than
#'   [a-z], [0-9] and \sQuote{-}.
#' @param fun [\code{function}] \cr
#'   Function definition. Must have a single parameter x.
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of paramter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Dimnesion of the targer space.
#' @param param.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set to describe (box) constraints for the function.
#' @param paretoSet [\code{function} | NULL] \cr
#'   Function, that returns n points randomly distributed on the true pareto set.
#' @return A \code{mooFunction} object.
#'
#' @export

mooFunction = function(name, id, fun, in.dim, out.dim,
  param.set, paretoSet) {
  
  assertCharacter(x = name, len = 1L, all.missing = FALSE)
  # FIXME: look at grepl patterns
  assertCharacter(x = id, len = 1L, all.missing = FALSE)#,
    #pattern = "^[:alpha:]+[[:alnum:]_-]*$")
  assertFunction(fun, args = getParamIds(param.set))
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  assertClass(param.set, "ParamSet")
  
  if (!is.null(paretoSet))
    assertFunction(paretoSet, args = "n")
  
  # check that fun fullfills in.dim and out.dim
  valid.point = sampleValue(param.set)
  
  value = do.call(fun, valid.point)
  if (length(value) != out.dim)
    stopf("Function has wrong out.dim, expect %d but got %d.", out.dim, length(value))
  
  # FIXME: generate 1 paretoFront and check for correct dim and only non-dom points
  
  structure(
    function(...)
      evalMooFunction(fun, param.set, ...),
    name = name,
    id = id,
    in.dim = in.dim,
    out.dim = out.dim,
    class = c("mooFunction", class(fun)),
    param.set = param.set,
    paretoSet = paretoSet
    )
}
