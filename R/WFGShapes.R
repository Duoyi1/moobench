#' WFG Shapes
#'
#' \code{wfgShapeLinear} is the linear pareto front. \cr
#' \code{wfgShapeConvex} is the convex pareto front. \cr
#' \code{wfgShapeConcave} is the concave pareto front. \cr
#' \code{wfgShapeMixed} is a shape of the pareto front that has some convex and concave parts. \cr
#' \code{wfgShapeDisconnected} is a shape of the pareto front that is not continuous. \cr
#'
#' @param dim [\code{integer(1)}] \cr 
#'   Most shape function differ in the first (dim = 1), the last (dim = out.dim)
#'   and the remainingout dimension. Indicates for which function the shape
#'   function shall be returned.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @param A [\code{integer(1)}] \cr 
#'   Number of parts of the pareto front. 
#' @param alpha [\code{numeric(1)}] \cr
#'   Must be > 0.
#'   The overall form of the pareto front, if alpha > 1 then it is more convex, 
#'   if alpha < 1 then more concave. When alpha = 1 the overall shape is linear.
#' @param beta [\code{numeric(1)}] \cr
#'   Must be > 0.
#'   Where the discontinuities are. A larger value moves it to larger values of the first objective.
#'   
#' @return A \code{wfgShapeFunction}.
#' 
#' @name WFGShapes
#' 
#' @aliases wfgShape WFGShape

#' @rdname WFGShapes
wfgShapeLinear = function(dim, out.dim) {
  dim = asCount(dim, positive = TRUE)
  out.dim = asCount(out.dim, positive = TRUE)
  if (dim > out.dim)
    stopf("You set out.dim = %i and dim = %i, but out.dim must be greater than dim!.",
      out.dim, dim)
  
  if (dim == 1L)
    shape.function = function(x) prod(x)
  else if (dim == out.dim)
    shape.function = function(x) 1 - x[1L]
  else
    shape.function = function(x) prod(x[1:(out.dim - dim)]) * (1 - x[out.dim - dim + 1])
  
  shape.function = addClasses(shape.function, "wfgShapeFunction")
  return(shape.function)
}

#' @rdname WFGShapes
wfgShapeConvex = function(dim, out.dim) {
  dim = asCount(dim, positive = TRUE)
  out.dim = asCount(out.dim, positive = TRUE)
  if (dim > out.dim)
    stopf("You set out.dim = %i and dim = %i, but out.dim must be greater than dim!.",
      out.dim, dim)
  
  if (dim == 1L)
    shape.function = function(x) prod(1 - cos(x * pi / 2))
  else if (dim == out.dim)
    shape.function = function(x) 1 - sin(x[1L] * pi / 2)
  else
    shape.function = function(x) prod(1 - cos(x[1:(out.dim - dim)] * pi / 2)) * 
        (1 - sin(x[out.dim - dim + 1] * pi / 2))

  shape.function = addClasses(shape.function, "wfgShapeFunction")
  return(shape.function)  
}

#' @rdname WFGShapes
wfgShapeConcave = function(dim, out.dim) {
  dim = asCount(dim, positive = TRUE)
  out.dim = asCount(out.dim, positive = TRUE)
  if (dim > out.dim)
    stopf("You set out.dim = %i and dim = %i, but out.dim must be greater than dim!.",
      out.dim, dim)
  
  if (dim == 1L)
    shape.function = function(x) prod(sin(x * pi / 2))
  else if (dim == out.dim)
    shape.function = function(x) cos(x[1L] * pi / 2)
  else
    shape.function = function(x) prod(sin(x[1:(out.dim - dim)] * pi / 2)) * 
        (cos(x[out.dim - dim + 1] * pi / 2))
  
  shape.function = addClasses(shape.function, "wfgShapeFunction")
  return(shape.function)
}

#' @rdname WFGShapes
wfgShapeMixed = function(dim, out.dim, alpha, A) {
  dim = asCount(dim, positive = TRUE)
  out.dim = asCount(out.dim, positive = TRUE)
  if (dim > out.dim)
    stopf("You set out.dim = %i and dim = %i, but out.dim must be greater than dim!.",
      out.dim, dim)
  assertNumber(alpha, lower = 0)
  A = asCount(A, positive = TRUE)
  
  tmp = 2 * A * pi
  shape.function = function(x) c((1 - x[1L] - cos(tmp * x[1L] + pi / 2) / tmp)^alpha)
  
  shape.function = addClasses(shape.function, "wfgShapeFunction")
  return(shape.function)
}

#' @rdname WFGShapes
wfgShapeDisconnected = function(dim, out.dim, alpha, beta, A) {
  dim = asCount(dim, positive = TRUE)
  out.dim = asCount(out.dim, positive = TRUE)
  if (dim > out.dim)
    stopf("You set out.dim = %i and dim = %i, but out.dim must be greater than dim!.",
      out.dim, dim)
  assertNumber(alpha, lower = 0)
  assertNumber(beta, lower = 0)
  A = asCount(A, positive = TRUE)
  
  shape.function = function(x) c(1 - x[1L]^alpha * cos(A * x[1L]^beta * pi)^2)
  
  shape.function = addClasses(shape.function, "wfgShapeFunction")
  return(shape.function)
}