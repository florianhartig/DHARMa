#' creates Poisson data overdispersion and random intercept
plotSimulatedResiduals <- function(residualObject){

  oldpar <- par(mfrow = c(1,2))
  hist(residuals, breaks = 50)
  ord <- order(fitted(fittedModel))
  plot(log(fitted(fittedModel)[ord]), residuals[ord], pch = 3)
  par(oldpar)
}


