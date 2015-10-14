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


renderPlotMooFunction = function(x, resolution = 33L, plot.vars = 1:2, const.values = NULL, ...) {
  
  in.dim = getInDim(x)
  
  if (in.dim < 2 || getOutDim(x) != 2) {
    stop("Only functions with InDim >= 2 and OutDim = 2 can be plotted!")
  }
  
  assertIntegerish(resolution, lower = 2L, len = 1L)
  assertIntegerish(plot.vars, lower = 1L, upper = in.dim, len = 2L)
  if (!is.null(const.values))  assertNumeric(const.values, len = in.dim - 2L)
  
  
  renderBasicPlot = function(dat, title, legend = FALSE, ...) {
    dat$f1 = BBmisc::normalize(dat$f1, method = "range", range = c(0, 1))
    dat$f2 = BBmisc::normalize(dat$f2, method = "range", range = c(0, 1))
    
    dat = reshape(dat, direction = "long", varying = list(c("f1", "f2")), timevar = "fill")
    dat$fill = as.factor(dat$fill)
    levels(dat$fill) = c("f1", "f2")
    #dat[is.na(dat$z1), ]$fill = rep("NA", sum(is.na(dat)))

    dat1 = dat[dat$fill == "f1", ]
    dat2 = dat[dat$fill == "f2", ]
    p = ggplot(data = dat1, aes = aes(x = x1, y = x2))
    p = p + geom_tile(data = dat1, aes(x = x1, y = x2, fill = fill, alpha = f1))
    p = p + geom_tile(data = dat2, aes(x = x1, y = x2, fill = fill, alpha = f1 - 0.25))
    p = p + scale_alpha_continuous(range = c(0, 0.75), guide = FALSE, na.value = 0.5)
    p = p + scale_fill_manual(values = c("blue", "red"), name = "variable", 
      guide = FALSE, na.value = "orange")
    p = p + scale_x_continuous(expand = c(0, 0))
    p = p + scale_y_continuous(expand = c(0, 0))
    p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), axis.line = element_blank())
    p = p + ggtitle(title)
    
    if(legend == TRUE) {
      p = p + xlab("f1") + ylab("f2")
    } else {
      p = p + xlab(paste("x", plot.vars[1L], sep = "")) + ylab(paste("x", plot.vars[2L], sep = ""))
    }
    
    return(p)
  }
  
  g_legend = function(a.gplot) { 
    tmp = ggplot_gtable(ggplot_build(a.gplot)) 
    leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend = tmp$grobs[[leg]] 
    return(legend)
  } 
  
  fun = x
  par.set = getParamSet(fun)
  lower = getLower(par.set)
  upper = getUpper(par.set)
  name = getName(fun)
  
  if (is.null(const.values)) {
    const.values = lower[-plot.vars]
  }
  
  grid = expand.grid(seq(lower[plot.vars[1L]], upper[plot.vars[1L]], length.out = resolution), 
    seq(lower[plot.vars[2L]], upper[plot.vars[2L]], length.out = resolution))
  grid2 = matrix(nrow = resolution ^ 2, ncol = getInDim(fun))
  grid2[, plot.vars] = as.matrix(grid)
  grid2[, -plot.vars] = matrix(const.values, nrow = resolution ^ 2, ncol = in.dim - 2L)
  
  z = apply(grid2, 1, fun)
  
  dat = cbind(grid, t(z))
  names(dat) = c("x1", "x2", "f1", "f2")  

  p = renderBasicPlot(dat, title = name, ...) 
  
  # add pareto set
  pareto = as.data.frame(getParetoSet(fun, n = 500L))
  if (nrow(pareto) > 0) {
    names(pareto) = c("x1", "x2")
    p = p + geom_point(data = pareto, aes(x = x1, y = x2, shape = "pareto set"))
    # extract legend from plot
    p.tmp = p + scale_shape_manual(name = "", values = c(16))
    p.legend2 = g_legend(p.tmp)
    p = p + scale_shape_manual(guide = FALSE, values = c(16))
  } else {
    p.legend2 = NULL
  }

  # legend
  lower.legend = c(min(dat$f1, na.rm = TRUE), min(dat$f2, na.rm = TRUE))
  upper.legend = c(max(dat$f1, na.rm = TRUE), max(dat$f2, na.rm = TRUE))
  grid.legend = expand.grid(seq(lower.legend[1L], upper.legend[1L], length.out = 50L), 
    seq(lower.legend[2L], upper.legend[2L], length.out = 50L))
  dat.legend = cbind(grid.legend, grid.legend)
  names(dat.legend) = c("x1", "x2", "f1", "f2")  
  p.legend1 = renderBasicPlot(dat.legend, title = "legend", legend = TRUE, ...)
  
  return(list(p = p, p.legend1 = p.legend1, p.legend2 = p.legend2))
}





