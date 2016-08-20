#' Simulated p-values for internal testing
#' @param overdispersion amount of overdispersion
#' @param n replicates
#' @param alpha significance level
#' @export 
#' @return list of various named objects. Within each, order is dispersion_glmer, overdisp_fun, DHARMa
benchmarkTypeI_II <- function(dispersionValues = 0, n = 10, alpha = 0.05, ...){
  
    values = list()

    positiveDharmaOmnibus= numeric(length(dispValues))
    positiveParametric= numeric(length(dispValues))
    positiveDharmaNonparametric = numeric(length(dispValues))
    
    for(j in 1:length(dispersionValues)){
  
      out = matrix(nrow = n, ncol = 3)
      
      for (i in 1:n){
        
        testData = createData(sampleSize = 250, fixedEffects = c(1,0.2,0.1,0.02), quadraticFixedEffects = c(0.1,-0.4,0.2,-0.1), overdispersion = dispersionValues[j], family = poisson())
        
        fittedModel <- glmer(observedResponse ~ Environment1 + I(Environment1^2) + Environment2 + I(Environment2^2) + Environment3 + I(Environment3^2) + Environment4 + I(Environment4^2) + (1|group) , family = "poisson", data = testData)
    
        
        simulationOutput <- simulateResiduals(fittedModel = fittedModel, ...)
        x = testUniformDistribution(simulationOutput)
        out[i,1] = x$p.value    
        
        out[i,2] = testOverdispersionParametric(model = fittedModel)$p.value
        
        simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T, ...)
        x = testOverdispersion(simulationOutput, plot = F, print = F)
        out[i,3] = x$p.value    
    
      }
      
      
      sig <- function(x) mean(x < alpha)
      
      means = colMeans(out)
      significant = apply(out, 2, sig)
      values[[i]] = list(pValues = out, means = means)
      
      positiveDharmaOmnibus[i] = significant[1]
      positiveParametric[i] = significant[2]
      positiveDharmaNonparametric[i] = significant[3]
   }
    
  out = list(values = values, positiveDharmaOmnibus= positiveDharmaOmnibus, positiveParametric = positiveParametric , positiveDharmaNonparametric = positiveDharmaNonparametric )
    
    
  return(out)
    
  
  
}
