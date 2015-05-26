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
#' @param lower.bounds [\code{numeric(in.dim)}] \cr
#'   Lower bounds of the parameter space.
#' @param upper.bounds [\code{numeric(out.dim)}] \cr
#'   Upper bounds of the parameter space.
#' @param pareto.set [\code{function} | NULL] \cr
#'   Function, that returns the true pareto set.
#' @param pareto.front [\code{function} | NULL] \cr
#'   Function, that returns the true pareto front.
#' @return A \code{moo_function} object.
#'
#' @export

mooFunction = function(name, id, fun, in.dim, out.dim,
  lower.bounds, upper.bounds, pareto.front, pareto.set) {
  
  assertCharacter(x = name, len = 1L, all.missing = FALSE)
  # FIXME: loog at grepl patterns
  assertCharacter(x = id, len = 1L, all.missing = FALSE)#,
    #pattern = "^[:alpha:]+[[:alnum:]_-]*$")
  assertFunction(fun, args = "x")
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  assertNumeric(lower.bounds, finite = TRUE, any.missing = FALSE, len = in.dim)
  assertNumeric(upper.bounds, finite = TRUE, any.missing = FALSE, len = in.dim)
  if (any(upper.bounds - lower.bounds <= 0))
    stop("Some upper bound is less or equal to its lower bound.")
  if (!is.null(pareto.set))
    assertFunction(pareto.set)
  if (!is.null(pareto.front))
    assertFunction(pareto.front)
  
  # check that fun fullfills in.dim and out.dim
  valid.point = (upper.bounds + lower.bounds) / 2
  value = fun(valid.point)
  if (length(value) != out.dim)
    stopf("Function has wrong out.dim, expect %d but got %d.", out.dim, length(value))
  
  # FIXME: check for pareto.set and pareto.front have correct dim
  
  
  
  structure(
    fun,
    name = name,
    id = id,
    in.dim = in.dim,
    out.dim = out.dim,
    class = c("mooFunction", class(fun)),
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.front = pareto.front,
    pareto.set = pareto.set
    )
}
