#' DHARMa standard residual plots
#'
#' This S3 function creates standard plots for the simulated residuals contained in an object of class DHARMa, using \code{\link{plotQQunif}} (left panel) and \code{\link{plotResiduals}} (right panel)
#' 
#' @param x an object of class DHARMa with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plot.DHARMa, but can be changed when using plotResiduals.
#' @param title The title for both panels (plotted via mtext, outer = T)
#' 
#' @details The function creates a plot with two panels. The left panel is a uniform qq plot (calling \code{\link{plotQQunif}}), and the right panel shows residuals against predicted values (calling \code{\link{plotResiduals}}), with outliers highlighted in red. 
#' 
#' Very briefly, we would expect that a correctly specified model shows:
#'
#' a) a straight 1-1 line, as well as n.s. of the displayed tests in the qq-plot (left) -> evidence for an the correct overall residual distribution (for more details on the interpretation of this plot, see \code{\link{plotQQunif}})
#'
#' b) visual homogeneity of residuals in both vertical and horizontal direction, as well as n.s. of quantile tests in the res ~ predictor plot (for more details on the interpretation of this plot, see \code{\link{plotResiduals}})
#'
#' Deviations from these expectations can be interpreted similar to a linear regression. See the vignette for detailed examples.
#' 
#' Note that, unlike \code{\link{plotResiduals}}, plot.DHARMa command uses the default rank = T.
#'
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @import graphics
#' @import utils
#' @export
plot.DHARMa <- function(x, title = "DHARMa residual", ...){

  oldpar <- par(mfrow = c(1,2), oma = c(0,1,2,1))
  on.exit(par(oldpar))
  
  plotQQunif(x)
  plotResiduals(x, ...)

  mtext(title, outer = T)
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
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @note This function is deprecated. Use \code{\link{plot.DHARMa}}
#'
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @export
plotSimulatedResiduals <- function(simulationOutput, ...){
  message("plotSimulatedResiduals is deprecated, please switch your code to simply using the plot() function")
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
#' @details the function calls qqunif from the R package gap to create a quantile-quantile plot for a uniform distribution, and overlays tests for particular distributional problems as specified.
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
plotQQunif <- function(simulationOutput, testUniformity = T, testOutliers = T, testDispersion = T, ...){
  
  a <- list(...)
  a$pch = checkDots("pch", 2, ...)
  a$bty = checkDots("bty", "n", ...)
  a$logscale = checkDots("logscale", F, ...)
  a$col = checkDots("col", "black", ...)
  a$main = checkDots("main", "QQ plot residuals", ...)
  a$cex.main = checkDots("cex.main", 1, ...)
  a$xlim = checkDots("xlim", c(0,1), ...)
  a$ylim = checkDots("ylim", c(0,1), ...)

  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")
  
  do.call(gap::qqunif, append(list(simulationOutput$scaledResiduals), a))

  if(testUniformity == TRUE){
    temp = testUniformity(simulationOutput, plot = F)
    legend("topleft", 
           c(paste("KS test: p=", round(temp$p.value, digits = 5)), 
             paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), 
           text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")
    
  }

  if(testOutliers == TRUE){
    temp = testOutliers(simulationOutput, plot = F)
    legend("bottomright", 
           c(paste("Outlier test: p=", round(temp$p.value, digits = 5)), 
             paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")
    
  }

  if(testDispersion == TRUE){
    temp = testDispersion(simulationOutput, plot = F)
    legend("center", 
           c(paste("Dispersion test: p=", round(temp$p.value, digits = 5)), 
             paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), 
           text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")
  }

}



#' Generic res ~ pred scatter plot with spline or quantile regression on top
#'
#' The function creates a generic residual plot with either spline or quantile regression to highlight patterns in the residuals. Outliers are highlighted in red.
#'
#' @param simulationOutput on object, usually a DHARMa object, from which residual values can be extracted. Alternatively, a vector with residuals or a fitted model can be provided, which will then be transformed into a DHARMa object.
#' @param form optional predictor against which the residuals should be plotted. Default is to used the predicted(simulationOutput)
#' @param quantreg whether to perform a quantile regression based on \code{\link{testQuantiles}} or a smooth spline around the mean. Default NULL chooses T for nObs < 2000, and F otherwise.
#' @param rank if T, the values provided in form will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed. If form is a factor, this has no effect.
#' @param asFactor should a numeric predictor provided in form be treated as a factor. Default is to choose this for < 10 unique values, as long as enough predictions are available to draw a boxplot.
#' @param smoothScatter if T, a smooth scatter plot will plotted instead of a normal scatter plot. This makes sense when the number of residuals is very large. Default NULL chooses T for nObs > 10000, and F otherwise.
#' @param quantiles for a quantile regression, which quantiles should be plotted
#' @param ... additional arguments to plot / boxplot.
#' @details The function plots residuals against a predictor (by default against the fitted value, extracted from the DHARMa object, or any other predictor).
#'
#' Outliers are highlighted in red (for information on definition and interpretation of outliers, see \code{\link{testOutliers}}).
#'
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional) quantile regression of the residuals, by default for the 0.25, 0.5 and 0.75 quantiles. As the residuals should be uniformly distributed for a correctly specified model, the theoretical expectations for these regressions are straight lines at 0.25, 0.5 and 0.75, which are displayed as dashed black lines on the plot. Some deviations from these expectations are to be expected by chance, however, even for a perfect model, especially if the sample size is small. The function therefore tests if deviation of the fitted quantile regression from the expectation is significant, using  \code{\link{testQuantiles}}. If so, the significant quantile regression will be highlighted as red, and a warning will be displayed in the plot. 
#' 
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. This is default for n > 2000.
#' 
#' If form is a factor, a boxplot will be plotted instead of a scatter plot. The distribution for each factor level should be uniformly distributed, so the box should go from 0.25 to 0.75, with the median line at 0.5 (within-group ). To test if deviations from those expecations are significant, KS-tests per group and a Levene test for homogeneity of variances is performed. See \code{\link{testCategorical}} for details. 
#' 
#' @note if nObs > 10000, the scatter plot is replaced by graphics::smoothScatter
#' 
#' @return if quantile tests are performed, the function returns them invisibly.
#'
#' @seealso \code{\link{plotQQunif}}, \code{\link{testQuantiles}}, \code{\link{testOutliers}}
#' @example inst/examples/plotsHelp.R
#' @export
plotResiduals <- function(simulationOutput, form = NULL, quantreg = NULL, rank = T, asFactor = NULL, smoothScatter = NULL, quantiles = c(0.25, 0.5, 0.75), ...){


  ##### Checks #####

  a <- list(...)
  a$ylab = checkDots("ylab", "DHARMa residual", ...)
  a$xlab = checkDots("xlab", ifelse(is.null(form), "Model predictions", 
                                    gsub(".*[$]","",deparse(substitute(form)))), ...)
  if(rank == T) a$xlab = paste(a$xlab, "(rank transformed)")

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)
  res = simulationOutput$scaledResiduals
  if(inherits(form, "DHARMa"))stop("DHARMa::plotResiduals > argument form cannot be of class DHARMa. Note that the syntax of plotResiduals has changed since DHARMa 0.3.0. See ?plotResiduals.")

  pred = ensurePredictor(simulationOutput, form)

  ##### Rank transform and factor conversion#####

  if(!is.factor(pred)){

    if (rank == T){
      pred = rank(pred, ties.method = "average")
      pred = pred / max(pred)
      a$xlim = checkDots("xlim", c(0,1), ...)
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

  # Note to self: wrapped in do.call because of the check dots, needs to be consolidate, e.g. for testCategorical

  # categorical plot
  if(is.factor(pred)){
    testCategorical(simulationOutput = simulationOutput, catPred = pred, quantiles = quantiles)
  }
  # smooth scatter
  else if (smoothScatter == TRUE) {
    defaultCol = ifelse(res == 0 | res == 1, 2,blackcol)
    do.call(graphics::smoothScatter, append(list(x = pred, y = res , ylim = c(0,1), axes = FALSE, colramp = colorRampPalette(c("white", "darkgrey"))),a))
    points(pred[defaultCol == 2], res[defaultCol == 2], col = "red", cex = 0.5)
    
    axis(1)
    axis(2, at=c(0, quantiles, 1))
  }
  # normal plot
  else{
    defaultCol = ifelse(res == 0 | res == 1, 2,blackcol)
    defaultPch = ifelse(res == 0 | res == 1, 8,1)
    a$col = checkDots("col", defaultCol, ...)
    a$pch = checkDots("pch", defaultPch, ...)
    do.call(plot, append(list(res ~ pred, ylim = c(0,1), axes = FALSE), a))
    
    axis(1)
    axis(2, at=c(0, quantiles, 1))
  }

  ##### Quantile regressions #####

  main = checkDots("main", ifelse(is.null(form), "Residual vs. predicted", "Residual vs. predictor"), ...)
  out = NULL

  if(is.numeric(pred)){
    if(quantreg == F){
      title(main = main, cex.main = 1)
      abline(h = quantiles, col = "black", lwd = 0.5, lty = 2)
      try({
        lines(smooth.spline(pred, res, df = 10), lty = 2, lwd = 2, col = "red")
        abline(h = 0.5, col = "red", lwd = 2)
      }, silent = T)
    }else{

      out = testQuantiles(simulationOutput, pred, quantiles = quantiles, plot = F)


      if(any(out$pvals < 0.05, na.rm = TRUE)){
        main = paste(main, "Quantile deviations detected (red curves)", sep ="\n")
        if(out$p.value <= 0.05){
          main = paste(main, "Combined adjusted quantile test significant", sep ="\n")
        } else {
          main = paste(main, "Combined adjusted quantile test n.s.", sep ="\n")
        }
        maincol = "red"
      } else {
        main = paste(main, "No significant problems detected", sep ="\n")
        maincol = "black"
      }


      title(main = main, cex.main = 0.8,
            col.main = maincol)

      for(i in 1:length(quantiles)){

        lineCol = ifelse(out$pvals[i] <= 0.05 & !(is.na(out$pvals[i])), "red", "black")
        filCol = ifelse(out$pvals[i] <= 0.05 & !(is.na(out$pvals[i])), "#FF000040", "#00000020")

        abline(h = quantiles[i], col = lineCol, lwd = 0.5, lty = 2)
        polygon(c(out$predictions$pred, rev(out$predictions$pred)),
                c(out$predictions[,2*i] - out$predictions[,2*i+1], rev(out$predictions[,2*i] + out$predictions[,2*i+1])),
                col = "#00000020", border = F)
        lines(out$predictions$pred, out$predictions[,2*i], col = lineCol, lwd = 2)
      }

      # legend("bottomright", c(paste("Quantile test: p=", round(out$p.value, digits = 5)), paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(out$p.value < 0.05, "red", "black" ), bty="n")

    }
  }
  invisible(out)
}

x = 0.01
x <= 0.05 & !(is.na(x))


#' Ensures the existence of a valid predictor to plot residuals against
#'
#' @param simulationOutput a DHARMa simulation output or an object that can be converted into a DHARMa simulation output
#' @param predictor an optional predictor. If no predictor is provided, will try to extract the fitted value
#' @keywords internal
ensurePredictor <- function(simulationOutput,
                            predictor = NULL){
  if(!is.null(predictor)){

    if(length(predictor) != length(simulationOutput$scaledResiduals)) stop("DHARMa: residuals and predictor do not have the same length. The issue is possibly that you have NAs in your predictor that were removed during the model fit. Remove the NA values from your predictor.")
    
    if(is.character(predictor)) {
      predictor = factor(predictor)
      warning("DHARMa:::ensurePredictor: character string was provided as predictor. DHARMa has converted to factor automatically. To remove this warning, please convert to factor before attempting to plot with DHARMa.")
    }
    
  } else {

    predictor = simulationOutput$fittedPredictedResponse
    if(is.null(predictor)) stop("DHARMa: can't extract predictor from simulationOutput, and no predictor provided")
  }
  return(predictor)
}




#plot(simulationOutput)

#plot(simulationOutput$observedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")

#plot(simulationOutput$observedResponse, simulationOutput$fittedPredictedResponse - simulationOutput$observedResponse)

#plot(cumsum(sort(simulationOutput$scaledResiduals)))


#plotConventionalResiduals(fittedModel)


#' Conventional residual plot
#'
#' Convenience function to draw conventional residual plots
#'
#' @param fittedModel a fitted model object
#' @export
plotConventionalResiduals <- function(fittedModel){
  opar <- par(mfrow = c(1,3), oma = c(0,1,2,1))
  on.exit(par(opar))
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



