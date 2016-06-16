#' creates Poisson data overdispersion and random intercept
plotSimulatedResiduals <- function(simulationOutput, quantreg = F){
  
  
  pred = simulationOutput$fittedPredictedResponse
  
  resid = simulationOutput$scaledResiduals
  
  dat = data.frame(pred, resid)
  
  
  par(mfrow = c(1,2), oma = c(0,1,2,1))
  
  gap::qqunif(simulationOutput$scaledResiduals,pch=2,bty="n", logscale = F, col = "black", cex = 0.6, main = "QQ plot residuals", cex.main = 1)
  #hist(simulationOutput$scaledResiduals, main = "Distribution of scaled residuals", breaks = 50, freq = F)
  #lines(density(simulationOutput$scaledResiduals, na.rm = T, from = 0, to = 1, kernel = "rectangular", bw = 0.01, cut = 0.01), col = "red")
  
  plot(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, xlab = "Predicted", ylab = "Residual", main = "Residual vs. predicted\n lines should match", cex.main = 1)
  

  

  if(quantreg == F){
    
    lines(smooth.spline(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, df = 10), lty = 2, lwd = 2, col = "red")
    
    abline(h = 0.5, col = "red", lwd = 2)

  }else{
    
    require(quantreg)
    dat <- plyr::arrange(dat,pred)
    fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.5,data = dat)
    lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "red", lwd = 2)
    abline(h = 0.5, col = "red", lwd = 2)
    
    fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.25,data = dat)
    lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "green", lwd = 2, lty =2)
    abline(h = 0.25, col = "green", lwd = 2, lty =2)
    
    fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.75,data = dat)
    lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "blue", lwd = 2, lty = 2)
    abline(h = 0.75, col = "blue", lwd = 2, lty =2)   
  }
  
  

  
  mtext("DHARMa scaled residual plots", outer = T)
}

#' Plots a generic residual plot with spline 
plotResiduals <- function(pred, res){
  
  plot(pred, res)
  
  lines(smooth.spline(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, df = 10), lty = 2, lwd = 2, col = "red")
  
  abline(h = 0.5, col = "red", lwd = 2)
  
}



#plotSimulatedResiduals(simulationOutput)

#plot(simulationOutput$observedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")

#plot(simulationOutput$observedResponse, simulationOutput$fittedPredictedResponse - simulationOutput$observedResponse)

#plot(cumsum(sort(simulationOutput$scaledResiduals)))

plotConventionalResiduals <- function(fittedModel){
  par(mfrow = c(1,3), oma = c(0,1,2,1))
  plot(predict(fittedModel), resid(fittedModel, type = "deviance"), main = "Deviance" , ylab = "Residual", xlab = "predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "pearson") , main = "Pearson", ylab = "Residual", xlab = "predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "response") , main = "Raw residuals" , ylab = "Residual", xlab = "predicted")  
  mtext("Conventional residual plots", outer = T)
}

#plotConcentionalResiduals(fittedModel)

