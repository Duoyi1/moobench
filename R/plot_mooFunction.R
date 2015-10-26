#' Plot a mooFunction.
#'
#' @param x [\code{function}] \cr 
#'  A \code{\link{mooFunction}}.
#' @param resolution [\code{integer(1)}] \cr
#'  Resolution for the plots. Default is 33.
#' @param plot.vars [\code{integer(2)}]\cr
#'  Indices for the two variables that should be plotted. Default are the first 
#'  two variables.
#' @param const.values [\code{numeric}] \cr
#'  Numeric vector with length "in.dim - 2" (one entry for each variable that is 
#'  not plotted). All variables which are not mentioned in \code{plot.vars} will 
#'  be set to a constant value given by \code{const.values}. By default the lower
#'  bounds of the variables are used.
#' @param ... Ignored.
#' 
#' @export


plot.mooFunction = function(x, resolution = 64L, plot.vars = 1:2, const.values = NULL, ...) {
  plots = renderPlotMooFunction(x, ...)
  if (!is.null(plots$p.legend2)) {
    p.legend = gridExtra::arrangeGrob(plots$p.legend1, plots$p.legend2, nrow = 2, 
      heights = c(0.75, 0.25))
  } else {
    p.legend = plots$p.legend1
  }
  gridExtra::grid.arrange(plots$p, p.legend, widths = c(0.75, 0.25))
}


renderPlotMooFunction = function(x, resolution = 64L, plot.vars = 1:2, const.values = NULL, ...) {
  
  in.dim = getInDim(x)
  
  # checks
  if (in.dim < 2 || getOutDim(x) != 2) {
    stop("Only functions with InDim >= 2 and OutDim = 2 can be plotted!")
  }
  assertIntegerish(resolution, lower = 2L, len = 1L)
  assertIntegerish(plot.vars, lower = 1L, upper = in.dim, len = 2L)
  if (!is.null(const.values))
    assertNumeric(const.values, len = in.dim - 2L)
  
  
  # function to render the main and the legend plot
  renderBasicPlot = function(dat, title, legend = FALSE, ...) {
    dat$f1 = BBmisc::normalize(dat$f1, method = "range", range = c(0, 1))
    dat$f2 = BBmisc::normalize(dat$f2, method = "range", range = c(0, 1))
    
    dat = reshape(dat, direction = "long", varying = list(c("f1", "f2")), timevar = "fill")
    dat$fill = as.factor(dat$fill)
    levels(dat$fill) = c("f1", "f2")
    dat[is.na(dat$f1), "fill"] = NA

    p = ggplot(aes = aes_string(x = "x1", y = "x2"))
    
    dat1 = dat[dat$fill == "f1", ]
    p = p + geom_tile(data = dat1, aes_string(x = "x1", y = "x2", fill = "fill", alpha = "f1"))
    
    dat2 = dat[dat$fill == "f2", ]
    p = p + geom_tile(data = dat2, aes_string(x = "x1", y = "x2", fill = "fill", alpha = "f1" - 0.25))
    
    dat3 = dat[is.na(dat$fill), ]
    if (nrow(dat3) > 0L) {
      dat3$f1 = 0.05
      p = p + geom_tile(data = dat3, aes_string(x = "x1", y = "x2", fill = "fill", alpha = "f1"))
    }
    
    p = p + scale_alpha_continuous(range = c(0, 0.75), guide = FALSE, na.value = 0.5)
    p = p + scale_fill_manual(values = c("blue", "red"), name = "variable", 
      guide = FALSE, na.value = "green")
    p = p + scale_x_continuous(expand = c(0, 0))
    p = p + scale_y_continuous(expand = c(0, 0))
    p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), axis.line = element_blank())
    p = p + ggtitle(title)
    p
    
    if (legend == TRUE) {
      p = p + xlab("f1") + ylab("f2")
    } else {
      p = p + xlab(paste("x", plot.vars[1L], sep = "")) + ylab(paste("x", plot.vars[2L], sep = ""))
    }
    
    return(p)
  }
  
  fun = x
  par.set = getParamSet(fun)
  lower = getLower(par.set)
  upper = getUpper(par.set)
  name = getName(fun)
  
  if (is.null(const.values)) {
    const.values = lower[-plot.vars]
  }
  
  # generate grids
  grid = expand.grid(seq(lower[plot.vars[1L]], upper[plot.vars[1L]], length.out = resolution), 
    seq(lower[plot.vars[2L]], upper[plot.vars[2L]], length.out = resolution))
  grid2 = matrix(nrow = resolution ^ 2, ncol = getInDim(fun))
  grid2[, plot.vars] = as.matrix(grid)
  grid2[, -plot.vars] = matrix(const.values, nrow = resolution ^ 2, ncol = in.dim - 2L)
  
  z = try(apply(grid2, 1, fun), silent = TRUE)
  if (inherits(z, "try-error")) {
    stop("Please create function f with on.infeasible = 'NA'.")
  }
  
  dat = cbind(grid, t(z))
  names(dat) = c("x1", "x2", "f1", "f2")  

  # render main plot
  p = renderBasicPlot(dat, title = name, ...) 
  
  # add pareto set
  pareto = as.data.frame(getParetoSet(fun, n = 500L))
  if (nrow(pareto) > 0) {
    names(pareto) = c("x1", "x2")
    p = p + geom_point(data = pareto, aes_string(x = "x1", y = "x2", shape = "pareto set"))
    p = p + scale_shape_manual(guide = FALSE, values = c(16))
  }

  # function to extract legend from plot
  g_legend = function(a.gplot) { 
    tmp = ggplot_gtable(ggplot_build(a.gplot)) 
    leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend = tmp$grobs[[leg]] 
    return(legend)
  } 
  
  # generate small extra plot which is only used to extract the legend
  dat.tmp = data.frame(x = c(1,2), y = c(1,1), fill = as.factor(c(1,1)))
  p.tmp = ggplot(data = dat.tmp, aes_string(x = "x", y = "y", fill = "fill", shape = "fill")) + 
    geom_tile(alpha = 0.25) + 
    geom_point(colour = "black")
  p.tmp = p.tmp + scale_fill_manual(name = "", values = c("green"), labels = "infeasible value") + 
    scale_shape_manual(name = "  ", values = c(16), labels = "pareto front") +
    guides(fill = guide_legend(override.aes = list(shape = NA)))
  # extract legend from plot
  p.legend2 = g_legend(p.tmp)
  
  # legend plot
  lower.legend = c(min(dat$f1, na.rm = TRUE), min(dat$f2, na.rm = TRUE))
  upper.legend = c(max(dat$f1, na.rm = TRUE), max(dat$f2, na.rm = TRUE))
  grid.legend = expand.grid(seq(lower.legend[1L], upper.legend[1L], length.out = 50L), 
    seq(lower.legend[2L], upper.legend[2L], length.out = 50L))
  dat.legend = cbind(grid.legend, grid.legend)
  names(dat.legend) = c("x1", "x2", "f1", "f2")  
  p.legend1 = renderBasicPlot(dat.legend, title = "legend", legend = TRUE, ...)
  
  return(list(p = p, p.legend1 = p.legend1, p.legend2 = p.legend2))
}





