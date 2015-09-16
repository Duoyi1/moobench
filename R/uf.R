#' UF test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which UF function? Valid values are 1, 2, ..., 10
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two for UF 1-7 functions and three for UF 8-10 functions.
#' @return A \code{mooFunction}.
#' 
#' @references
#' Zhang, Qingfu ; Zhou, Aimin ; Zhaoy, Shizheng ; Suganthany, Ponnuthurai N. ; 
#' Liu, Wudong ; Tiwari, Santosh: Multiobjective optimization Test Instances 
#' for the CEC 2009 Special Session and Competition / University of Essex and 
#' Nanyang Technological University. 2008. - Technical Report CES-487
#' 
#' @aliases uf UF
#' 
#' @export
#' 
#' @examples
#'  x = runif(30)
#'  fun = generateUF(id = 1, in.dim = 30L, out.dim = 2L) 
#'  fun(x)

generateUF = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id %in% c(8:10) && out.dim != 3L)
    stopf("UF%i supports only out.dim = 3.", id)
  if (id %in% c(1:7) && out.dim != 2L)
    stopf("UF%i supports only out.dim = 2.", id)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:10)
  
  switch(id,
    generateUF1(in.dim, out.dim), 
    generateUF2(in.dim, out.dim), 
    generateUF3(in.dim, out.dim), 
    generateUF4(in.dim, out.dim), 
    generateUF5(in.dim, out.dim), 
    generateUF6(in.dim, out.dim), 
    generateUF7(in.dim, out.dim), 
    generateUF8(in.dim, out.dim), 
    generateUF9(in.dim, out.dim), 
    generateUF10(in.dim, out.dim)
    )
}
