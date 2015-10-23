#' CF test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which CF function? Valid values are 1, 2, ..., 10
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two for CF 1-7 functions and three for CF 8-10 functions.
#' @param on.infeasible [\code{character}] \cr
#'   What should happen if infeasible values are evaluated? Possible values are
#'   stop (code stops with error message) and NA (NA value is returned).
#' @return A \code{mooFunction}.
#' 
#' @references
#' Zhang, Qingfu ; Zhou, Aimin ; Zhaoy, Shizheng ; Suganthany, Ponnuthurai N. ; 
#' Liu, Wudong ; Tiwari, Santosh: Multiobjective optimization Test Instances 
#' for the CEC 2009 Special Session and Competition / University of Essex and 
#' Nanyang Technological University. 2008. - Technical Report CES-487
#' 
#' @aliases cf CF
#' 
#' @export
#' 
#' @examples
#'  #x = runif(30)
#'  #fun = generateCF(id = 1, in.dim = 30L, out.dim = 2L) 
#'  #fun(x)

generateCF = function(id, in.dim = 30L, out.dim = 2L, on.infeasible = "stop") {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id %in% c(8:10) && out.dim != 3L)
    stopf("CF%i supports only out.dim = 3.", id)
  if (id %in% c(8:10) && in.dim < 5L)
    stopf("CF%i supports only in.dim >= 5.", id)
  
  if (id %in% c(1:7) && out.dim != 2L)
    stopf("CF%i supports only out.dim = 2.", id)
  if (id %in% c(1:5) && in.dim < 3L)
    stopf("CF%i supports only in.dim >= 3.", id)
  if (id %in% c(6:7) && in.dim < 4L)
    stopf("CF%i supports only in.dim >= 4.", id)
  
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:10)
  
  switch(id,
    generateCF1(in.dim, out.dim, on.infeasible), 
    generateCF2(in.dim, out.dim, on.infeasible), 
    generateCF3(in.dim, out.dim, on.infeasible), 
    generateCF4(in.dim, out.dim, on.infeasible), 
    generateCF5(in.dim, out.dim, on.infeasible), 
    generateCF6(in.dim, out.dim, on.infeasible), 
    generateCF7(in.dim, out.dim, on.infeasible), 
    generateCF8(in.dim, out.dim, on.infeasible), 
    generateCF9(in.dim, out.dim, on.infeasible), 
    generateCF10(in.dim, out.dim, on.infeasible)
  )
}
