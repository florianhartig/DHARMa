#' DHARMa standard residual plots
#' 
#' This function creates standard plots for the simulated residuals
#' @param x an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param rank if T (default), the values of pred will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed. 
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @details The function creates two plots. To the left, a qq-uniform plot to detect deviations from overall uniformity of the residuals (calling \code{\link{plotQQunif}}), and to the right, a plot of residuals against predicted values (calling \code{\link{plotResiduals}}). Outliers are highlighted in red (for more on oultiers, see \code{\link{testOutliers}}). For a correctly specified model, we would expect 
#' 
#' a) a straight 1-1 line in the uniform qq-plot -> evidence for an overal uniform (flat) distribution of the residuals
#' 
#' b) uniformity of residuals in the vertical direction in the res against predictor plot
#' 
#' Deviations of this can be interpreted as for a linear regression. See the vignette for detailed examples. 
#' 
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predited values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small. See further comments on this plot, it's interpreation and options, in \code{\link{plotResiduals}}
#' 
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. This is default for n > 2000. 
#' 
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @import graphics
#' @import utils
#' @export
plot.DHARMa <- function(x, rank = TRUE, ...){
  
  oldpar <- par(mfrow = c(1,2), oma = c(0,1,2,1))
  
  plotQQunif(x)
  
  xlab = checkDots("xlab", ifelse(rank, "Model predictions (rank transformed)", "Model predictions"), ...)

  plotResiduals(simulationOutput = x, xlab = xlab, rank = rank, ...)
  
  mtext("DHARMa residual diagnostics", outer = T)
  
  par(oldpar)
}


#' Histogram of DHARMa residuals
#' 
#' The function produces a histogram from a DHARMa output
#' 
#' @param x a DHARMa simulation output (class DHARMa)
#' @param breaks breaks for hist() function
#' @param col col for hist bars
#' @param main plot main
#' @param xlab plot xlab
#' @param cex.main plot cex.main
#' @param ... other arguments to be passed on to hist
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
hist.DHARMa <- function(x, 
                        breaks = seq(-0.02, 1.02, len = 53), 
                        col = c("red",rep("lightgrey",50), "red"),
                        main = "Hist of DHARMa residuals",
                        xlab = "Residuals (outliers are marked red)",
                        cex.main = 1,
                        ...){
  
  x = ensureDHARMa(x, convert = T)
  
  val = x$scaledResiduals
  val[val == 0] = -0.01
  val[val == 1] = 1.01

  hist(val, breaks = breaks, col = col, main = main, xlab = xlab, cex.main = cex.main, ...)
}


#' DHARMa standard residual plots
#' 
#' DEPRECATED, use plot() instead
#' 
#' @param simulationOutput an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @note This function is deprecated. Use \code{\link{plot.DHARMa}}
#' 
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @export
plotSimulatedResiduals <- function(simulationOutput, ...){
  message("plotSimulatedResiduals is deprecated, switch your code to using the plot function")
  plot(simulationOutput, ...)
}


#' Quantile-quantile plot for a uniform distribution
#' 
#' The function produces a uniform quantile-quantile plot from a DHARMa output
#' 
#' @param simulationOutput a DHARMa simulation output (class DHARMa)
#' @param testUniformity if T, the function \code{\link{testUniformity}} will be called and the result will be added to the plot
#' @param testOutliers if T, the function \code{\link{testOutliers}} will be called and the result will be added to the plot
#' @param testDispersion if T, the function \code{\link{testDispersion}} will be called and the result will be added to the plot
#' @param ... arguments to be passed on to \code{\link[gap]{qqunif}}
#' 
#' @details the function calls qqunif from the R package gap to create a quantile-quantile plot for a uniform distribution.  
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
plotQQunif <- function(simulationOutput, testUniformity = T, testOutliers = T, testDispersion = T, ...){
  
  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  gap::qqunif(simulationOutput$scaledResiduals,pch=2,bty="n", logscale = F, col = "black", cex = 0.6, main = "QQ plot residuals", cex.main = 1, ...)
  
  if(testUniformity == TRUE){
    temp = testUniformity(simulationOutput, plot = F)
    legend("topleft", c(paste("KS test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")     
  }
  
  if(testOutliers == TRUE){
    temp = testOutliers(simulationOutput, plot = F)
    legend("bottomright", c(paste("Outlier test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")     
  }
  
  if(testDispersion == TRUE){
    temp = testDispersion(simulationOutput, plot = F)
    legend("center", c(paste("Dispersion test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")     
  }
  
}


#' Generic residual plot with either spline or quantile regression
#' 
#' The function creates a generic residual plot with either spline or quantile regression to highlight patterns in the residuals. Outliers are highlighted in red
#' 
#' @param simulationOutput usually a DHARMa object, from which residual values can be extracted. Alternatively, a vector with residuals or a fitted model can be provided, which will then be transformed into a DHARMa object
#' @param predictor either the predictor variable against which the residuals should be plotted, or a DHARMa object, in which case res ~ pred is plotted
#' @param quantreg whether to perform a quantile regression on 0.25, 0.5, 0.75 on the residuals. If F, a spline will be created instead. Default NULL chooses T for nObs < 2000, and F otherwise. 
#' @param rank if T, the values of pred will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed. If pred is a factor, this has no effect. 
#' @param asFactor should a numeric predictor be treated as a factor. Default is to choose this for < 10 unique predictions, as long as enough predictions are available to draw a boxplot.
#' @param smoothScatter if T, a smooth scatter plot will plotted instead of a normal scatter plot. This makes sense when the number of residuals is very large. Default NULL chooses T for nObs < 10000, and F otherwise.
#' @param quantiles for a quantile regression, which quanties should be plotted 
#' @param ... additional arguments to plot / boxplot. 
#' @details The function plots residuals against a predictor (by default against the fitted value, extracted from the DHARMa object, or any other predictor). 
#' 
#' Outliers are highlighted in red (for information on definition and interpretation of outliers, see \code{\link{testOutliers}}). 
#' 
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles (default) in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line). 
#' 
#' Assymptotically (i.e. for lots of data / residuals), if the model is correct, theoretical and the empirical quantiles should be identical (i.e. dashed and solid lines should match). A p-value for the deviation is calculated for each quantile line. Significant deviations are highlighted by red color. 
#' 
#' If pred is a factor, a boxplot will be plotted instead of a scatter plot. The distribution for each factor level should be uniformly distributed, so the box should go from 0.25 to 0.75, with the median line at 0.5. Again, chance deviations from this will increases when the sample size is smaller. You can run null simulations to test if the deviations you see exceed what you would expect from random variation. If you want to create box plots for categorical predictors (e.g. because you only have a small number of unique numberic predictor values), you can convert your predictor with as.factor(pred)
#' 
#' @note The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. 
#' 
#' @seealso \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @export
plotResiduals <- function(simulationOutput, predictor = NULL, quantreg = NULL, rank = F, asFactor = NULL, smoothScatter = NULL, quantiles = c(0.25, 0.5, 0.75), ...){
  
  
  ##### Checks #####
  
  ylab = checkDots("ylab", "Standardized residual", ...)
  
  simulationOutput = ensureDHARMa(simulationOutput, convert = T)
  res = simulationOutput$scaledResiduals
  pred = ensurePredictor(simulationOutput, predictor)
  
  ##### Rank transform and factor conversion#####
  
  if(!is.factor(pred)){

    if (rank == T){
      pred = rank(pred, ties.method = "average")
      pred = pred / max(pred)          
    } 
        
    nuniq = length(unique(pred))
    ndata = length(pred)  
    if(is.null(asFactor)) asFactor = (nuniq == 1) | (nuniq < 10 & ndata / nuniq > 10)
    if (asFactor) pred = factor(pred)
  }

  ##### Residual scatter plots #####
  
  if(is.null(quantreg)) if (length(res) > 2000) quantreg = FALSE else quantreg = TRUE
  
  switchScatter = 10000
  if(is.null(smoothScatter)) if (length(res) > switchScatter) smoothScatter = TRUE else smoothScatter = FALSE

  blackcol = rgb(0,0,0, alpha = max(0.1, 1 - 3 * length(res) / switchScatter))
  
  defaultCol = ifelse(res == 0 | res == 1, 2,blackcol)
  defaultPch = ifelse(res == 0 | res == 1, 8,1)   

  col = checkDots("col", defaultCol, ...)
  pch = checkDots("pch", defaultPch, ...)
  
  if(is.factor(pred)){
    plot(res ~ pred, ylim = c(0,1), axes = FALSE, ...)
  } 
  else if (smoothScatter == TRUE) {
    smoothScatter(pred, res , ylim = c(0,1), axes = FALSE, colramp = colorRampPalette(c("white", "darkgrey")))
    points(pred[defaultCol == 2], res[defaultCol == 2], col = "red", cex = 0.5)
  }
  else{
    plot(res ~ pred, ylim = c(0,1), axes = FALSE, col = col, pch = pch, ylab = ylab, ...)
  } 
  
  axis(1)
  axis(2, at=c(0, 0.25, 0.5, 0.75, 1))
  
  ##### Quantile regressions #####
  
  main = checkDots("main", "Residual vs. predicted", ...)
  out = NULL
  
  if(is.numeric(pred)){
    if(quantreg == F){
      title(main = main, cex.main = 1)
      abline(h = c(0.25, 0.5, 0.75), col = "black", lwd = 0.5, lty = 2)
      try({
        lines(smooth.spline(pred, res, df = 10), lty = 2, lwd = 2, col = "red")
        abline(h = 0.5, col = "red", lwd = 2)
      }, silent = T)
    }else{
      
      out = testQuantiles(simulationOutput, pred, quantiles = quantiles, plot = F)
      
      if(any(out$pvals < 0.05)){
        main = paste(main, "Quantile deviations detected (red curves)", sep ="\n")
        if(out$p.value <= 0.05){
          main = paste(main, "Combined adjusted quantile test signficant", sep ="\n")
        } else {
          main = paste(main, "Combined adjusted quantile test n.s.", sep ="\n")
        }
        maincol = "red"
      } else {
        main = paste(main, "No signficiant problems detected", sep ="\n")
        maincol = "black"
      }
      
      title(main = main, cex.main = 0.8, 
            col.main = maincol)
      
      for(i in 1:length(quantiles)){
        
        lineCol = ifelse(out$pvals[i] <= 0.05, "red", "black")
        filCol = ifelse(out$pvals[i] <= 0.05, "#FF000040", "#00000020")
        
        abline(h = quantiles[i], col = lineCol, lwd = 0.5, lty = 2)
        polygon(c(out$predictions$pred, rev(out$predictions$pred)),
                c(out$predictions[,2*i] - out$predictions[,2*i+1], rev(out$predictions[,2*i] + out$predictions[,2*i+1])), 
                col = "#00000020", border = F)
        lines(out$predictions$pred, out$predictions[,2*i], col = lineCol, lwd = 2)
      }
      
      # legend("bottomright", c(paste("Quantile test: p=", round(out$p.value, digits = 5)), paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(out$p.value < 0.05, "red", "black" ), bty="n")  
      
    }
  }
  return(out)
}



#plot(simulationOutput)

#plot(simulationOutput$observedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")

#plot(simulationOutput$observedResponse, simulationOutput$fittedPredictedResponse - simulationOutput$observedResponse)

#plot(cumsum(sort(simulationOutput$scaledResiduals)))


#plotConcentionalResiduals(fittedModel)


#' Conventional residual plot
#' 
#' Convenience function to draw conventional residual plots
#' 
#' @param fittedModel a fitted model object
#' @export
plotConventionalResiduals <- function(fittedModel){
  par(mfrow = c(1,3), oma = c(0,1,2,1))
  plot(predict(fittedModel), resid(fittedModel, type = "deviance"), main = "Deviance" , ylab = "Residual", xlab = "Predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "pearson") , main = "Pearson", ylab = "Residual", xlab = "Predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "response") , main = "Raw residuals" , ylab = "Residual", xlab = "Predicted")  
  mtext("Conventional residual plots", outer = T)
}




# 
# 
# if(quantreg == F){
#   
#   lines(smooth.spline(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, df = 10), lty = 2, lwd = 2, col = "red")
#   
#   abline(h = 0.5, col = "red", lwd = 2)
#   
# }else{
#   
#   #library(gamlss)
#   
#   # qrnn
#   
#   # http://r.789695.n4.nabble.com/Quantile-GAM-td894280.html
#   
#   #require(quantreg)
#   #dat <- plyr::arrange(dat,pred)
#   #fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.5,data = dat)
#   
#   probs = c(0.25, 0.50, 0.75)
#   
#   w <- p <- list()
#   for(i in seq_along(probs)){
#     capture.output(w[[i]] <- qrnn::qrnn.fit(x = as.matrix(simulationOutput$fittedPredictedResponse), y = as.matrix(simulationOutput$scaledResiduals), n.hidden = 4, tau = probs[i], iter.max = 1000, n.trials = 1, penalty = 1))
#     p[[i]] <- qrnn::qrnn.predict(as.matrix(sort(simulationOutput$fittedPredictedResponse)), w[[i]])
#   }
#   
#   
#   
#   #plot(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, xlab = "Predicted", ylab = "Residual", main = "Residual vs. predicted\n lines should match", cex.main = 1)
#   
#   #lines(sort(simulationOutput$fittedPredictedResponse), as.vector(p[[1]]), col = "red")
#   
#   matlines(sort(simulationOutput$fittedPredictedResponse), matrix(unlist(p), nrow = length(simulationOutput$fittedPredictedResponse), ncol = length(p)), col = "red", lty = 1)
#   
#   #     as.vector(p[[1]])
#   #     
#   #     
#   #     lines(simulationOutput$fittedPredictedResponse,p[[1]], col = "red", lwd = 2)
#   #     abline(h = 0.5, col = "red", lwd = 2)
#   #     
#   #     fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.25,data = dat)
#   #     lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "green", lwd = 2, lty =2)
#   #     abline(h = 0.25, col = "green", lwd = 2, lty =2)
#   #     
#   #     fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.75,data = dat)
#   #     lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "blue", lwd = 2, lty = 2)
#   #     abline(h = 0.75, col = "blue", lwd = 2, lty =2)   
# }



# New nlme plots ----------------------------------------------------------

## copied/modified from lme4 
## lme4 copied/modified from nlme

##' split, on the nm call, the rhs of a formula into a list of subformulas
splitFormula <- function(form, sep = "/")
{
  if (inherits(form, "formula") ||
      mode(form) == "call" && form[[1]] == as.name("~"))
    splitFormula(form[[length(form)]], sep = sep)
  else if (mode(form) == "call" && form[[1]] == as.name(sep))
    do.call(c, lapply(as.list(form[-1]), splitFormula, sep = sep))
  else if (mode(form) == "(")
    splitFormula(form[[2]], sep = sep)
  else if (length(form))
    list(asOneSidedFormula(form))
  ## else
  ##  NULL
}

## Recursive version of all.vars
allVarsRec <- function(object)
{
  if (is.list(object)) {
    unlist(lapply(object, allVarsRec))
  } else {
    all.vars(object)
  }
}

## simple version of getData.gnls from nlme
## but we *should* and *can* work with environment(formula(.))
getData.merMod <-  function(object)
{
  mCall <- object@call
  eval(mCall$data, environment(formula(object)))
}

asOneFormula <-
  ## Constructs a linear formula with all the variables used in a
  ## list of formulas, except for the names in omit
  function(..., omit = c(".", "pi"))
  {
    names <- unique(allVarsRec(list(...)))
    names <- names[is.na(match(names, omit))]
    if (length(names))
      as.formula(paste("~", paste(names, collapse = "+"))) # else NULL
  }

getIDLabels <- function(object, form=formula(object)) {
  mf <- factorize(form,model.frame(object))
  if (length(ff <- findbars(form))>0) {
    grps <- lapply(ff,"[[",3)
  } else {
    grps <- form[[2]]
  }
  if (identical(grps,quote(.obs))) return(seq(fitted(object)))
  fList <- lapply(grps,function(x) eval(x,mf))
  do.call(interaction,fList)
}

## TESTING
## lme4:::getIDLabels(fm1)

## Return the formula(s) for the groups associated with object.
## The result is a one-sided formula unless asList is TRUE in which case
## it is a list of formulas, one for each level.
getGroupsFormula <- function(object, asList = FALSE, sep = "+")
  UseMethod("getGroupsFormula")

getGroupsFormula.default <-
  ## Return the formula(s) for the groups associated with object.
  ## The result is a one-sided formula unless asList is TRUE in which case
  ## it is a list of formulas, one for each level.
  function(object, asList = FALSE, sep = "/")
  {
    form <- formula(object)
    if (!inherits(form, "formula")){
      stop("\"Form\" argument must be a formula")
    }
    form <- form[[length(form)]]
    if (!((length(form) == 3) && (form[[1]] == as.name("|")))) {
      ## no conditioning expression
      return(NULL)
    }
    ## val <- list( asOneSidedFormula( form[[ 3 ]] ) )
    val <- splitFormula(asOneSidedFormula(form[[3]]), sep = sep)
    names(val) <- unlist(lapply(val, function(el) deparse(el[[2]])))
    #  if (!missing(level)) {
    #    if (length(level) == 1) {
    #      return(val[[level]])
    #    } else {
    #      val <- val[level]
    #    }
    #  }
    if (asList) as.list(val)
    else as.formula(paste("~", paste(names(val), collapse = sep)))
  }

getGroupsFormula.merMod <- function(object,asList=FALSE, sep="+") {
  if (asList) {
    lapply(names(object@flist),asOneSidedFormula)
  } else {
    asOneSidedFormula(paste(names(object@flist),collapse=sep))
  }
}

getCovariateFormula <- function (object)
{
  form <- formula(object)
  if (!(inherits(form, "formula"))) {
    stop("formula(object) must return a formula")
  }
  form <- form[[length(form)]]
  if (length(form) == 3 && form[[1]] == as.name("|")) {
    form <- form[[2]]
  }
  eval(substitute(~form))
}

getResponseFormula <-
  function(object)
  {
    ## Return the response formula as a one sided formula
    form <- formula(object)
    if (!(inherits(form, "formula") && (length(form) == 3))) {
      stop("\"Form\" must be a two sided formula")
    }
    as.formula(paste("~", deparse(form[[2]])))
  }

##' diagnostic plots for merMod fits
##' @param x a fitted [ng]lmer model
##' @param form an optional formula specifying the desired type of plot. Any
##' variable present in the original data frame used to obtain
##' \code{x} can be referenced. In addition, \code{x} itself can be
##' referenced in the formula using the symbol \code{"."}. Conditional
##'  expressions on the right of a \code{|} operator can be used to
##'  define separate panels in a lattice display. Default is
##' \code{resid(., type = "pearson") ~ fitted(.)}, corresponding to a plot
##' of the standardized residuals versus fitted values.
##' @param abline an optional numeric value, or numeric vector of length
##'   two. If given as a single value, a horizontal line will be added to the
##'   plot at that coordinate; else, if given as a vector, its values are
##'   used as the intercept and slope for a line added to the plot. If
##'   missing, no lines are added to the plot.
##' @param id an optional numeric value, or one-sided formula. If given as
##' a value, it is used as a significance level for a two-sided outlier
##' test for the standardized, or normalized residuals. Observations with
##'   absolute standardized (normalized) residuals greater than the \eqn{1-value/2}
##' quantile of the standard normal distribution are
##' identified in the plot using \code{idLabels}. If given as a one-sided
##'   formula, its right hand side must evaluate to a  logical, integer, or
##'   character vector which is used to identify observations in the
##'   plot. If missing, no observations are identified.
##' @param idLabels an optional vector, or one-sided formula. If given as a
##'   vector, it is converted to character and used to label the
##'   observations identified according to \code{id}. If given as a
##'    vector, it is converted to character and used to label the
##'    observations identified according to \code{id}. If given as a
##'    one-sided formula, its right hand side must evaluate to a vector
##'    which is converted to character and used to label the identified
##'    observations. Default is the interaction of all the grouping variables
##'    in the data frame.  The special formula
##' @param grid an optional logical value indicating whether a grid should
##'    be added to plot. Default depends on the type of lattice plot used:
##'    if \code{xyplot} defaults to \code{TRUE}, else defaults to
##'    \code{FALSE}.
##'  @param \dots optional arguments passed to the lattice plot function.
##' @details Diagnostic plots for the linear mixed-effects fit are obtained. The
##'  \code{form} argument gives considerable flexibility in the type of
##'  plot specification. A conditioning expression (on the right side of a
##'  \code{|} operator) always implies that different panels are used for
##'  each level of the conditioning factor, according to a lattice
##'  display. If \code{form} is a one-sided formula, histograms of the
##'  variable on the right hand side of the formula, before a \code{|}
##'  operator, are displayed (the lattice function \code{histogram} is
##'  used). If \code{form} is two-sided and both its left and
##'  right hand side variables are numeric, scatter plots are displayed
##'  (the lattice function \code{xyplot} is used). Finally, if \code{form}
##'  is two-sided and its left had side variable is a factor, box-plots of
##'  the right hand side variable by the levels of the left hand side
##'  variable are displayed (the lattice function  \code{bwplot} is used).
##' @author original version in \code{nlme} package by Jose Pinheiro and Douglas Bates
##' @examples
##' data(Orthodont,package="nlme")
##' fm1 <- lmer(distance ~ age + (age|Subject), data=Orthodont)
##' ## standardized residuals versus fitted values by gender
##' plot(fm1, resid(., scaled=TRUE) ~ fitted(.) | Sex, abline = 0)
##' ## box-plots of residuals by Subject
##' plot(fm1, Subject ~ resid(., scaled=TRUE))
##' ## observed versus fitted values by Subject
##' plot(fm1, distance ~ fitted(.) | Subject, abline = c(0,1))
##' ## residuals by age, separated by Subject
##' plot(fm1, resid(., scaled=TRUE) ~ age | Sex, abline = 0)

##' if (require(ggplot2)) {
##'     ## we can create the same plots using ggplot2 and the fortify() function
##'     fm1F <- fortify(fm1)
##'     ggplot(fm1F, aes(.fitted,.resid)) + geom_point(colour="blue") +
##'            facet_grid(.~Sex) + geom_hline(yintercept=0)
##'     ## note: Subjects are ordered by mean distance
##'     ggplot(fm1F, aes(Subject,.resid)) + geom_boxplot() + coord_flip()
##'     ggplot(fm1F, aes(.fitted,distance))+ geom_point(colour="blue") +
##'         facet_wrap(~Subject) +geom_abline(intercept=0,slope=1)
##'     ggplot(fm1F, aes(age,.resid)) + geom_point(colour="blue") + facet_grid(.~Sex) +
##'         geom_hline(yintercept=0)+geom_line(aes(group=Subject),alpha=0.4)+geom_smooth(method="loess")
##'     ## (warnings about loess are due to having only 4 unique x values)
##'     detach("package:ggplot2")
##' }
##' @S3method plot merMod
##' @method plot merMod
##' @export
plot.merMod <-
  function(x, form = resid(., type = "pearson") ~ fitted(.), abline,
           id = NULL, idLabels = NULL,
           grid, ...)
    ## Diagnostic plots based on residuals and/or fitted values
  {
    object <- x
    if (!inherits(form, "formula"))
      stop("\"form\" must be a formula")
    ## constructing data
    ## can I get away with using object@frame???
    allV <- all.vars(asOneFormula(form, id, idLabels))
    allV <- allV[is.na(match(allV,c("T","F","TRUE","FALSE",".obs")))]
    if (length(allV) > 0) {
      data <- getData(object)
      if (is.null(data)) {            # try to construct data
        alist <- lapply(as.list(allV), as.name)
        names(alist) <- allV
        alist <- c(list(as.name("data.frame")), alist)
        mode(alist) <- "call"
        data <- eval(alist, sys.parent(1))
      } else if (any(naV <- is.na(match(allV, names(data)))))
        stop(allV[naV], " not found in data")
    } else data <- NULL
    
    ## this won't do because there may well be variables we want
    ##  that were not in the model call
    
    ## data <- object@frame
    
    ## argument list
    dots <- list(...)
    args <- if (length(dots) > 0) dots else list()
    ## appending object to data, and adding observation-number variable
    data <- as.list(c(as.list(cbind(data,.obs=seq(nrow(data)))), . = list(object)))
    ## covariate - must always be present
    covF <- getCovariateFormula(form)
    .x <- eval(covF[[2]], data)
    if (!is.numeric(.x)) {
      stop("Covariate must be numeric")
    }
    argForm <- ~ .x
    argData <- data.frame(.x = .x, check.names = FALSE)
    if (is.null(args$xlab)) {
      if (is.null(xlab <- attr(.x, "label")))
        xlab <- deparse(covF[[2]])
      args$xlab <- xlab
    }
    
    ## response - need not be present
    respF <- getResponseFormula(form)
    if (!is.null(respF)) {
      .y <- eval(respF[[2]], data)
      if (is.null(args$ylab)) {
        if (is.null(ylab <- attr(.y, "label")))
          ylab <- deparse(respF[[2]])
        args$ylab <- ylab
      }
      argForm <- .y ~ .x
      argData[, ".y"] <- .y
    }
    
    ## groups - need not be present
    grpsF <- getGroupsFormula(form)
    if (!is.null(grpsF)) {
      ## ?? FIXME ???
      gr <- splitFormula(grpsF, sep = "*")
      for(i in seq_along(gr)) {
        auxGr <- all.vars(gr[[i]])
        for(j in auxGr)
          argData[[j]] <- eval(as.name(j), data)
      }
      argForm <-
        as.formula(paste(if (length(argForm) == 2)
          "~ .x |" else ".y ~ .x |",
          deparse(grpsF[[2]])))
    }
    ## adding to args list
    args <- c(list(argForm, data = argData), args)
    if (is.null(args$strip)) {
      args$strip <- function(...) strip.default(..., style = 1)
    }
    if (is.null(args$cex)) args$cex <- par("cex")
    if (is.null(args$adj)) args$adj <- par("adj")
    
    if (!is.null(id)) {       ## identify points in plot
      idResType <- "pearson"  ## diff from plot.lme: 'normalized' not available
      id <- switch(mode(id),
                   numeric = {
                     if (id <= 0 || id >= 1)
                       stop(shQuote("id")," must be between 0 and 1")
                     abs(resid(object, type = idResType))/sigma(object) >
                       -qnorm(id / 2)
                   },
                   call = eval(asOneSidedFormula(id)[[2]], data),
                   stop(shQuote("id")," can only be a formula or numeric.")
      )
      if (is.null(idLabels)) {
        idLabels <- getIDLabels(object)
      } else {
        if (inherits(idLabels,"formula")) {
          idLabels <- getIDLabels(object,idLabels)
        } else if (is.vector(idLabels)) {
          if (length(idLabels <- unlist(idLabels)) != length(id)) {
            stop("\"idLabels\" of incorrect length")
          }
        } else stop("\"idLabels\" can only be a formula or a vector")
      }
      ## DON'T subscript by id, will be done later
      idLabels <- as.character(idLabels)
    }
    
    ## defining abline, if needed
    if (missing(abline)) {
      abline <- if (missing(form)) # r ~ f
        c(0, 0) else NULL
    }
    
    #assign("id", id , where = 1)
    #assign("idLabels", idLabels, where = 1)
    #assign("abl", abline, where = 1)
    assign("abl", abline)
    
    ## defining the type of plot
    if (length(argForm) == 3) {
      if (is.numeric(.y)) {           # xyplot
        plotFun <- "xyplot"
        if (is.null(args$panel)) {
          args <- c(args,
                    panel = list(function(x, y, subscripts, ...)
                    {
                      x <- as.numeric(x)
                      y <- as.numeric(y)
                      dots <- list(...)
                      if (grid) panel.grid()
                      panel.xyplot(x, y, ...)
                      if (any(ids <- id[subscripts])){
                        ltext(x[ids], y[ids], idLabels[subscripts][ids],
                              cex = dots$cex, adj = dots$adj)
                      }
                      if (!is.null(abl)) {
                        if (length(abl) == 2) panel.abline(a = abl, ...) else panel.abline(h = abl, ...)
                      }
                    }))
        }
      } else {                                # assume factor or character
        plotFun <- "bwplot"
        if (is.null(args$panel)) {
          args <- c(args,
                    panel = list(function(x, y, ...)
                    {
                      if (grid) panel.grid()
                      panel.bwplot(x, y, ...)
                      if (!is.null(abl)) {
                        panel.abline(v = abl[1], ...)
                      }
                    }))
        }
      }
    } else {
      plotFun <- "histogram"
      if (is.null(args$panel)) {
        args <- c(args,
                  panel = list(function(x, ...)
                  {
                    if (grid) panel.grid()
                    panel.histogram(x, ...)
                    if (!is.null(abl)) {
                      panel.abline(v = abl[1], ...)
                    }
                  }))
      }
    }
    
    ## defining grid
    if (missing(grid)) {
      grid <- (plotFun == "xyplot")
    }
    # assign("grid", grid, where = 1)
    do.call(plotFun, as.list(args))
  }

## no longer defining `fortify` S3 generic

##' @rdname fortify
##' @S3method fortify lmerMod
##' @method fortify lmerMod
##' @export
##'   as function, not as S3 method, see ../man/fortify.Rd  :
fortify.merMod <- function(model, data=getData(model), ...) {
  
  ## FIXME: get influence measures via influence.ME?
  ##   (expensive, induces dependency ...)
  ## FIXME: different kinds of residuals?
  ## FIXME: deal with na.omit/predict etc.
  data$.fitted <- predict(model)
  data$.resid <- resid(model)
  data$.scresid <- resid(model,type="pearson",scaled=TRUE)
  data
}


## autoplot???

##  plot method for plot.summary.mer ... coefplot-style
##  horizontal, vertical? other options???
##  scale?
plot.summary.mer <- function(object, type="fixef", ...) {
  if(any(!type %in% c("fixef","vcov")))
    stop("'type' not yet implemented: ", type)
  stop("FIXME -- not yet implemented")
  
}

## TO DO: allow faceting formula
## TO DO: allow qqline to be optional
## TO DO (harder): steal machinery from qq.gam for better GLMM Q-Q plots
qqmath.merMod <- function(x, id=NULL, idLabels=NULL, ...) {
  ## if (!is.null(id) || !is.null(idLabels))
  ##  stop("id and idLabels options not yet implemented")
  values <- residuals(x,type="pearson",scaled=TRUE)
  data <- getData(x)
  ## DRY: copied from plot.merMod, should modularize/refactor
  if (!is.null(id)) {       ## identify points in plot
    id <- switch(mode(id),
                 numeric = {
                   if (id <= 0 || id >= 1)
                     stop(shQuote("id")," must be between 0 and 1")
                   as.logical(abs(values) > -qnorm(id / 2))
                 },
                 call = eval(asOneSidedFormula(id)[[2]], data),
                 stop(shQuote("id")," can only be a formula or numeric.")
    )
    if (is.null(idLabels)) {
      idLabels <- getIDLabels(x)
    } else {
      if (inherits(idLabels,"formula")) {
        idLabels <- getIDLabels(x,idLabels)
      } else if (is.vector(idLabels)) {
        if (length(idLabels <- unlist(idLabels)) != length(id)) {
          stop("\"idLabels\" of incorrect length")
        }
      } else stop("\"idLabels\" can only be a formula or a vector")
    }
    idLabels <- as.character(idLabels)
    
  }
  ## DON'T subscript by id, will be done later
  qqpanel <- function(x, subscripts, ...) {
    dots <- list(...)
    panel.qqmathline(x, ...)
    panel.qqmath(x, ...)
    if (any(ids <- id[subscripts])) {
      xs <- x[subscripts]
      pp <- setNames(ppoints(length(xs)),
                     names(sort(xs)))
      ## want to plot qnorm(pp) vs sort(x)
      ## ... but want to pick out the elements that corresponded
      ## to ids **before** sorting
      xx <- qnorm(pp)[names(xs)[ids]]
      yy <- sort(x)[names(xs)][ids] ## quantile(values, pp)[ids]
      ltext(xx,
            yy,
            idLabels[ids],
            cex = dots$cex, adj = dots$adj)
    }
  }
  qqmath(values, xlab = "Standard normal quantiles",
         ylab = "Standardized residuals",
         prepanel = prepanel.qqmathline,
         panel = qqpanel,
         ...)
}

## qqmath(~residuals(gm1)|cbpp$herd)






