#' LZ test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which LZ function? Valid values are 1, 2, ..., 9
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two for LZ 1-5, 7-9 functions and three for LZ 6 functions.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' Li, Hui ; Zhang, Qingfu: Multiobjective Optimization Problems With
#' Complicated Pareto Sets, MOEA/D and NSGA-II. In: IEEE Trans.
#' Evolutionary Computation 13 (2009), No. 2, pp. 284-302
#' 
#' @aliases lz LZ
#' @export
#'
#' @examples
#'  x = runif(30)
#'  fun = generateLZ(id = 1, in.dim = 30L, out.dim = 2L) 
#'  fun(x)

generateLZ = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id == 6 && out.dim != 3L)
      stop("LZ6 supports only out.dim = 3.")
  if (id == 6 && in.dim < 5L)
    stop("LZ6 supports only in.dim >= 5.")
  if (id != 6 && out.dim != 2L)
    stopf("LZ%i supports only out.dim = 2.", id)
  if (id != 6 && in.dim < 3L)
    stopf("LZ%i supports only in.dim >= 3.", id)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:9)

  switch(id,
    generateLZ1(in.dim, out.dim), 
    generateLZ2(in.dim, out.dim), 
    generateLZ3(in.dim, out.dim), 
    generateLZ4(in.dim, out.dim), 
    generateLZ5(in.dim, out.dim), 
    generateLZ6(in.dim, out.dim), 
    generateLZ7(in.dim, out.dim), 
    generateLZ8(in.dim, out.dim), 
    generateLZ9(in.dim, out.dim) 
  )
}
