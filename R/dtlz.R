#' DTLZ test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which DTLZ function? Valid values are 1, 2, ..., 7
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references
#' Deb, Kalyanmoy ; Thiele, Lothar ; Laumanns, Marco ; Zitzler, Eckart: 
#' Scalable multi-objective optimization test problems / 
#' Computer Engineering and Networks Laboratory (TIK), 
#' Swiss Federal Institute of Technology (ETH). 2002.
#' 
#' @aliases dtlz DTLZ
#' 
#' @examples
#'  x = runif(30)
#'  fun = generateDTLZ(id = 1, in.dim = 30L, out.dim = 2L) 
#'  fun(x)
#' @export

generateDTLZ = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:7)
  
  switch(id,
    generateDTLZ1(in.dim, out.dim),
    generateDTLZ2(in.dim, out.dim), 
    generateDTLZ3(in.dim, out.dim), 
    generateDTLZ4(in.dim, out.dim), 
    generateDTLZ5(in.dim, out.dim), 
    generateDTLZ6(in.dim, out.dim),
    generateDTLZ7(in.dim, out.dim)
    )
}
