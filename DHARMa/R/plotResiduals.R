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
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predited values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small. See further comments on this plot, and options, in \code{\link{plotResiduals}}
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
  
  xla = ifelse(rank, "Predicted values (rank transformed)", "Predicted values")
  
  plotResiduals(pred = x, residuals = NULL, xlab = xla, ylab = "Standardized residual", main = "Residual vs. predicted\n lines should match", rank = rank, ...)
  
  mtext("DHARMa scaled residual plots", outer = T)
  
  par(oldpar)
}


#' Histogram of DHARMa residuals
#' 
#' The function produces a histogram from a DHARMa output
#' 
#' @param x a DHARMa simulation output (class DHARMa)
#' @param ... arguments to be passed on to hist. Breaks and col are fixed. 
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
hist.DHARMa <- function(x, ...){
  val = x$scaledResiduals
  val[val == 0] = -0.01
  val[val == 1] = 1.01
  hist(val, breaks = seq(-0.02, 1.02, len = 53), col = c("red",rep("lightgrey",50), "red"), main = "Hist of DHARM residuals\nOutliers are marked red", ...)
}




#' DHARMa standard residual plots
#' 
#' DEPRECATED, use plot() instead
#' 
#' @param simulationOutput an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @note THis function is deprecated. Use \code{\link{plot.DHARMa}}
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
#' 
#' @details the function calls qqunif from the R package gap to create a quantile-quantile plot for a uniform distribution.  
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
plotQQunif <- function(simulationOutput, testUniformity = T, testOutliers = T){
  
  if(class(simulationOutput) != "DHARMa") stop("DHARMa::plotQQunif wrong argument, simulationOutput must be a DHARMa object!")

  gap::qqunif(simulationOutput$scaledResiduals,pch=2,bty="n", logscale = F, col = "black", cex = 0.6, main = "QQ plot residuals", cex.main = 1)
  
  if(testUniformity == TRUE){
    temp = testUniformity(simulationOutput, plot = F)
    legend("topleft", c(paste("KS test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")     
  }
  
  if(testOutliers == TRUE){
    temp = testOutliers(simulationOutput, plot = F)
    legend("bottomright", c(paste("Outlier test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")     
  }
}


#' Generic residual plot with either spline or quantile regression
#' 
#' The function creates a generic residual plot with either spline or quantile regression
#' 
#' @param pred either the predictor variable against which the residuals should be plotted, or a DHARMa object
#' @param residuals residuals values. Leave empty if pred is a DHARMa object
#' @param quantreg whether to perform a quantile regression on 0.25, 0.5, 0.75 on the residuals. If F, a spline will be created instead. Default NULL chooses T for nObs < 2000, and F otherwise. 
#' @param rank if T, the values of pred will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed. If pred is a factor, this has no effect. 
#' @param asFactor should the predictor variable converted into a factor
#' @param ... additional arguments to plot
#' @details Plots residuals against a predictor. Outliers are highlighted in red (for more on oultiers, see \code{\link{testOutliers}}). For a correctly specified model, we would expect uniformity in y direction when plotting against any predictor.
#' 
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predited values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small.
#' 
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. 
#' 
#' @note if pred is a factor, a boxplot will be plotted instead of a scatter plot. The distribution for each factor level should be uniformly distributed, so the box should go from 0.25 to 0.75, with the median line at 0.5. Again, chance deviations from this will increases when the sample size is smaller. You can run null simulations to test if the deviations you see exceed what you would expect from random variation. If you want to create box plots for categorical predictors (e.g. because you only have a small number of unique numberic predictor values), you can convert your predictor with as.factor(pred)
#' 
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @export
plotResiduals <- function(pred, residuals = NULL, quantreg = NULL, rank = FALSE, asFactor = FALSE, ...){
  
  # conversions from DHARMa 
  if(class(pred) == "DHARMa"){
    if (! is.null(residuals)) stop("DHARMa::plotResiduals - can't provide both a DHARMa object to pred and additional residuals")
    res = pred$scaledResiduals
    pred = pred$fittedPredictedResponse
  } else {
    if (is.null(residuals)) stop("DHARMa::plotResiduals - residual can only be NULL if pred is of class DHARMa")
    res = residuals
  }

  # CHECK FOR LENGTH AND NA PROBLEM
  if(length(pred) != length(res)) {
    if(any(is.na(pred))){
      stop("DHARMa::plotResiduals - residuals and predictor do not have the same length. The issue is likely that you have NAs in your predictor that were removed during the model fit. Remove the NA values from your predictor.")      
    } else {
      stop("DHARMa::plotResiduals - residuals and predictor do not have the same length. ")        
    }
  }

  if (asFactor) pred = as.factor(pred)
  
  if(!is.factor(pred)){
    nuniq = length(unique(pred))
    ndata = length(pred)
    if(nuniq < 10 & ndata / nuniq > 10) message("DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T")
    if(nuniq < 10 & ndata / nuniq > 10) message("DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T")
    # this rank tranforms the predictor
    if (rank == T){
      pred = rank(pred, ties.method = "average")
      pred = pred / max(pred)          
    } 
  } else {
    # if (rank == T) warning("DHARMa::plotResiduals - predictor is a factor, rank = T has no effect")
  }
  

  
  if(is.null(quantreg)) if (length(res) > 2000) quantreg = FALSE else quantreg = TRUE

  defaultCol = ifelse(res == 0 | res == 1, 2,1)   
  defaultPch = ifelse(res == 0 | res == 1, 8,1)   

  col = checkDots("col", defaultCol, ...)
  pch = checkDots("pch", defaultPch, ...)
  
  if(is.factor(pred)) plot(res ~ pred, ylim = c(0,1), axes = FALSE, ...)
  else plot(res ~ pred, ylim = c(0,1), axes = FALSE, col = col, pch = pch, ...)
  
  axis(1)
  axis(2, at=c(0, 0.25, 0.5, 0.75, 1))
  abline(h = c(0.25, 0.5, 0.75), col = "black", lwd = 0.5, lty = 2)
  
  if(is.numeric(pred)){
    if(quantreg == F){
      try({
        lines(smooth.spline(pred, res, df = 10), lty = 2, lwd = 2, col = "red")
        abline(h = 0.5, col = "red", lwd = 2)
      }, silent = T)
    }else{
      probs = c(0.25, 0.50, 0.75)
      w <- p <- list()
      for(i in seq_along(probs)){
        try({
          capture.output(w[[i]] <- qrnn::qrnn.fit(x = as.matrix(pred), y = as.matrix(res), n.hidden = 4, tau = probs[i], iter.max = 1000, n.trials = 1, penalty = 1))
          p[[i]] <- qrnn::qrnn.predict(as.matrix(sort(pred)), w[[i]])
        }, silent = T)
      }
      matlines(sort(pred), matrix(unlist(p), nrow = length(pred), ncol = length(p)), col = "red", lty = 1)
    }
  }else{

  }
  
  
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

