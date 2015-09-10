#' ZDT test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which ZDT function? Valid values are 1, 2, 3, 4, 6
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two for all ZDT functions.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' Zitzler, Eckart ; Deb, Kalyanmoy ; Thiele, Lothar: Comparison of 
#' Multiobjective Evolutionary Algorithms: Empirical Results. In: Evolutionary
#' Computation 8 (2000), pp. 173-195
#' 
#' @aliases zdt ZDT
#' 
#' @export
#' 
#' @examples
#'  x = runif(30)
#'  fun = generateZDT(id = 1, in.dim = 30L, out.dim = 2L) 
#'  fun(x)
 
generateZDT = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("ZDT supports only out.dim = 2.")
  
  if (in.dim < out.dim)
    stopf("YOu set out.dim = %i and in.dim = %i, but in.dim must be greater than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:6)
  
  switch(id,
    generateZDT1(in.dim, out.dim), 
    generateZDT2(in.dim, out.dim), 
    generateZDT3(in.dim, out.dim), 
    generateZDT4(in.dim, out.dim), 
    ,
    generateZDT6(in.dim, out.dim)
  )
}

