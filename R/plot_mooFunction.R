#' Plot a mooFunction.
#'
#' @param x [\code{function}] \cr 
#'  A \code{\link{mooFunction}}.
#' @param ... Ignored.
#' 
#' @export


plot.mooFunction = function(x, ...) {
  plots = renderPlotMooFunction(x, ...)
  if (!is.null(plots$p.legend2)) {
    p.legend = gridExtra::arrangeGrob(plots$p.legend1, plots$p.legend2, nrow = 2)
  } else {
    p.legend = plots$p.legend1
  }
  gridExtra::grid.arrange(plots$p, p.legend, widths = c(0.75, 0.25))
}


renderPlotMooFunction = function(x, resolution = 33L, ...) {
  
  if (getInDim(x) != 2 || getOutDim(x) != 2) {
    stop("Only functions with InDim = 2 and OutDim = 2 can be plotted!")
  }
  
  renderBasicPlot = function(dat, title, legend = FALSE, ...) {
    dat$z1 = BBmisc::normalize(dat$z1, method = "range", range = c(0, 1))
    dat$z2 = BBmisc::normalize(dat$z2, method = "range", range = c(0, 1))
    
    dat = reshape(dat, direction = "long", varying = list(c("z1", "z2")), timevar = "fill")
    dat$fill = as.factor(dat$fill)
    levels(dat$fill) = c("z1", "z2")

    dat1 = dat[dat$fill == "z1", ]
    dat2 = dat[dat$fill == "z2", ]
    p = ggplot(data = dat1, aes = aes(x = x, y = y))
    p = p + geom_tile(data = dat1, aes(x = x, y = y, fill = fill, alpha = z1))
    p = p + geom_tile(data = dat2, aes(x = x, y = y, fill = fill, alpha = z1 - 0.25))
    p = p + scale_alpha_continuous(range = c(0, 0.75), guide = FALSE)
    p = p + scale_fill_manual(values = c("blue", "red"), name = "variable", guide = FALSE)
    p = p + scale_x_continuous(expand = c(0, 0)) 
    p = p + scale_y_continuous(expand = c(0, 0))
    p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), axis.line = element_blank())
    p = p + ggtitle(title)
    
    if(legend == TRUE) {
      p = p + xlab("z1") + ylab("z2")
    }
    
    return(p)
  }
  
  g_legend = function(a.gplot) { 
    tmp = ggplot_gtable(ggplot_build(a.gplot)) 
    leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend = tmp$grobs[[leg]] 
    return(legend)
  } 
  
  par.set = getParamSet(x)
  lower = getLower(par.set)
  upper = getUpper(par.set)
  name = getName(x)
  
  grid = expand.grid(seq(lower[1L], upper[1L], length.out = resolution), 
    seq(lower[2L], upper[2L], length.out = resolution))
  z = apply(grid, 1, x)
  dat = cbind(grid[, 1:2], t(z))
  names(dat) = c("x", "y", "z1", "z2")  

  p = renderBasicPlot(dat, title = name, ...) 
  
  # add pareto set
  pareto = as.data.frame(getParetoSet(x))
  if (nrow(pareto) > 0) {
    names(pareto) = c("x", "y")
    p = p + geom_line(data = pareto, aes(x = x, y = y, linetype = "pareto set"))
    # extract legend from plot
    p.tmp = p + scale_linetype_manual(name = "", values = c(1))
    p.legend2 = g_legend(p.tmp)
    p = p + scale_linetype_manual(guide = FALSE, values = c(1))
  } else {
    p.legend2 = NULL
  }

  # legend
  lower.legend = c(min(dat$z1), min(dat$z2))
  upper.legend = c(max(dat$z1), max(dat$z2))
  grid.legend = expand.grid(seq(lower.legend[1L], upper.legend[1L], length.out = resolution), 
    seq(lower.legend[2L], upper.legend[2L], length.out = resolution))
  dat.legend = cbind(grid.legend, grid.legend)
  names(dat.legend) = c("x", "y", "z1", "z2")  
  p.legend1 = renderBasicPlot(dat.legend, title = "legend", legend = TRUE, ...)
  
  return(list(p = p, p.legend1 = p.legend1, p.legend2 = p.legend2))
}





