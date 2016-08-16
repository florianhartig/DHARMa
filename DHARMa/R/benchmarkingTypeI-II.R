#' Simulated p-values for internal testing
#' @param overdispersion amount of overdispersion
#' @param n replicates
#' @param alpha significance level
#' @export 
#' @return list of various named objects. Within each, order is dispersion_glmer, overdisp_fun, DHARMa
benchmarkTypeI_II <- function(overdispersion = 0, n = 20, alpha = 0.05){
  
  out = matrix(nrow = n, ncol = 3)
  
  for (i in 1:nrow(out)){
    
    testData = createData(sampleSize = 250, fixedEffects = c(1,0.2,0.1,0.02), quadraticFixedEffects = c(0.1,-0.4,0.2,-0.1), overdispersion = overdispersion, family = poisson())
    fittedModel <- glmer(observedResponse ~ Environment1 + I(Environment1^2) + Environment2 + I(Environment2^2) + Environment3 + I(Environment3^2) + Environment4 + I(Environment4^2) + (1|group) , family = "poisson", data = testData)
    
    
    out[i,1] = blmeco::dispersion_glmer(fittedModel)
    
    out[i,2] = overdisp_fun(fittedModel)[4]
    
    
    simulationOutput <- simulateResiduals(fittedModel = fittedModel)
    x = testUniformDistribution(simulationOutput)
    out[i,3] = x$p.value
  }
  
  
  sig <- function(x) mean(x < alpha)
  
  means = colMeans(out)
  significant = apply(out, 2, sig)
  
  return(list(pValues = out, means = means, significant = significant))
}
