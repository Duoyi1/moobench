#' Plot a mooFunction.
#'
#' @param x [\code{function}] \cr 
#'  A \code{\link{mooFunction}}.
#' @param ... Ignored.
#' 
#' @export


plot.mooFunction = function(x, ...) {
  p = renderPlotMooFunction(x, ...)
  print(p)
}


renderPlotMooFunction = function(x, resolution = 33L, ...) {
  
  if (getInDim(x) != 2 || getOutDim(x) != 2) {
    stop("Only functions with InDim = 2 and OutDim = 2 can be plotted!")
  }
  
  par.set = getParamSet(x)
  upper = getUpper(par.set)
  lower = getLower(par.set)
  
  grid = expand.grid(seq(lower[1L], upper[1L], length.out = resolution), 
    seq(lower[2L], upper[2L], length.out = resolution))
  z = apply(grid, 1, x)
  dat = cbind(grid, t(z))
  names(dat) = c("x", "y", "z1", "z2")  
  
  dat$z1 = BBmisc::normalize(dat$z1, method = "range", range = c(0, 1))
  dat$z2 = BBmisc::normalize(dat$z2, method = "range", range = c(0, 1))
  
  dat = reshape(dat, direction = "long", varying = list(c("z1", "z2")), timevar = "fill")
  dat$fill = as.factor(dat$fill)
  levels(dat$fill) = c("z1", "z2")
  dat1 = dat[dat$fill == "z1", ]
  dat2 = dat[dat$fill == "z2", ]
  
  
  pareto = as.data.frame(getParetoSet(x))
  names(pareto) = c("x", "y")
  
  p = ggplot(data = dat1, aes = aes(x = x, y = y))
  p = p + geom_tile(data = dat1, aes(x = x, y = y, fill = fill, alpha = z1))
  p = p + geom_tile(data = dat2, aes(x = x, y = y, fill = fill, alpha = z1 - 0.25))
  p = p + geom_line(data = pareto, aes(x = x, y = y))
 
  p = p + scale_alpha_continuous(range = c(0, 0.75), 
    labels = c("low", "", "", "", "", "high"), name = "value", 
    guide = guide_legend(reverse = TRUE))
  p = p + scale_fill_manual(values = c("blue", "red"), name = "variable")
  p = p + theme(panel.background = element_rect(fill = "white"))
  return(p)
  
}





