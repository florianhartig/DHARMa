#' DHARMa standard residual plots
#' 
#' This function creates standard plots for the simulated residuals
#' @param simulationOutput an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param quantreg whether to perform a quantile regression on 0.25, 0.5, 0.75. If F, a spline will be created instead
#' @details The function creates two plots. To the left, a qq-uniform plot to detect deviations from overall uniformity of the residuals, and to the right a plot of residuals against predicted values. For a correctly specified model we would expect 
#' 
#' a) a uniform (flat) distribution of the overall residuals, evidenced by a straight line in the qq-plot
#' 
#' b) uniformity in y direction if we plot against any predictor, including the predicted value.
#' 
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predited values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small.
#' 
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. 
#' 
#' @seealso \code{\link{plotResiduals}}
#' @import graphics
#' @import utils
#' @export
plotSimulatedResiduals <- function(simulationOutput, quantreg = T){

  oldpar <- par(mfrow = c(1,2), oma = c(0,1,2,1))
  
  gap::qqunif(simulationOutput$scaledResiduals,pch=2,bty="n", logscale = F, col = "black", cex = 0.6, main = "QQ plot residuals", cex.main = 1)

  plotResiduals(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, xlab = "Predicted value", ylab = "Standardized residual", main = "Residual vs. predicted\n quantile lines should be\n horizontal lines at 0.25, 0.5, 0.75", cex.main = 1, quantreg = quantreg)
  
  mtext("DHARMa scaled residual plots", outer = T)
  
  par(oldpar)
}


#' Generic residual plot with either spline or quantile regression
#' 
#' The function creates a generic residual plot with either spline or quantile regression
#' 
#' @param pred predictor variable
#' @param residual residual variable
#' @param quantreg should a quantile regression be performed. If F, a smooth spline will be plotted
#' @param ... additional arguments to plot
#' @details For a correctly specified model, we would expect uniformity in y direction when plotting against any predictor.
#' 
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predited values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small.
#' 
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. 
#' 
#' If the predictor is a factor (categorial), a boxplot will be created - with a uniform distrituion, the box should go from 0.25 to 0.75, with the median line at 0.5. Again, chance deviations from this will increases when the sample size is smaller
#'   
#' @name if predictor is a factor, a boxplot will be plotted instead of a scatter plot.
#' @seealso \code{\link{plotSimulatedResiduals}}
#' @export
plotResiduals <- function(pred, residual, quantreg = T, ...){
  
  res = residual 
  
  plot(res ~ pred, ...)
  
  if(is.numeric(pred)){
    if(quantreg == F){
      lines(smooth.spline(pred, res, df = 10), lty = 2, lwd = 2, col = "red")
      abline(h = 0.5, col = "red", lwd = 2)
    }else{
      probs = c(0.25, 0.50, 0.75)
      w <- p <- list()
      for(i in seq_along(probs)){
        capture.output(w[[i]] <- qrnn::qrnn.fit(x = as.matrix(pred), y = as.matrix(res), n.hidden = 4, tau = probs[i], iter.max = 1000, n.trials = 1, penalty = 1))
  p[[i]] <- qrnn::qrnn.predict(as.matrix(sort(pred)), w[[i]])
      }
      matlines(sort(pred), matrix(unlist(p), nrow = length(pred), ncol = length(p)), col = "red", lty = 1)
    }
  }
}



#plotSimulatedResiduals(simulationOutput)

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

