#' Simulated p-values for internal testing
#' @param overdispersion amount of overdispersion
#' @param n replicates
#' @param alpha significance level
#' @export 
#' @return list of various named objects. Within each, order is dispersion_glmer, overdisp_fun, DHARMa
benchmarkOverdispersion <- function(dispersionValues = 0, n = 10, alpha = 0.05, plot = T, parallel = F, ...){
  
    library(lme4)
  
    values = list()

    positiveDharmaOmnibus= numeric(length(dispersionValues))
    positiveParametric= numeric(length(dispersionValues))
    positiveDharmaNonparametric = numeric(length(dispersionValues))
    
    for(j in 1:length(dispersionValues)){
  
      out = matrix(nrow = n, ncol = 3)
      
      
      getP <- function(){
        testData = createData(sampleSize = 250, fixedEffects = c(1,0.2,0.1,0.02), quadraticFixedEffects = c(0.1,-0.4,0.2,-0.1), overdispersion = dispersionValues[j], family = poisson())
        
        fittedModel <- glmer(observedResponse ~ Environment1 + I(Environment1^2) + Environment2 + I(Environment2^2) + Environment3 + I(Environment3^2) + Environment4 + I(Environment4^2) + (1|group) , family = "poisson", data = testData)
        
        
        simulationOutput <- simulateResiduals(fittedModel = fittedModel, ...)
        out = rep(NA,3)
        x = testUniformity(simulationOutput)
        out[1] = x$p.value    
        
        out[2] = testOverdispersionParametric(model = fittedModel)$p.value
        
        simulationOutput <- simulateResiduals(fittedModel = simulationOutput$fittedModel, refit = T, n = 100, ...)
        x = testOverdispersion(simulationOutput, plot = F, print = F)
        out[3] = x$p.value
        return(out)
      }
      
        
      if (parallel == F){
        out = replicate(2, getP(), simplify = "array")
        out = t(out)
      }else{
        library(foreach)
        if (parallel == T | parallel == "auto"){
          cores <- parallel::detectCores() - 1
          message("parallel, set cores automatically to ", cores)
        } else if (is.numeric(parallel)){
          cores <- parallel
          message("parallel, set number of cores by hand to ", cores)
        } else stop("wrong argument to parallel")
        
        cl <- parallel::makeCluster(cores)
        doParallel::registerDoParallel(cl)
        
        out <- foreach::foreach(i=1:n, .packages=c("lme4", "DHARMa") , .combine = rbind) %dopar% getP()
        
        parallel::stopCluster(cl = cl)
        
      }
    
      sig <- function(x) mean(x < alpha)
      
      means = colMeans(out)
      significant = apply(out, 2, sig)
      values[[j]] = list(pValues = out, means = means)
      
      positiveDharmaOmnibus[j] = significant[1]
      positiveParametric[j] = significant[2]
      positiveDharmaNonparametric[j] = significant[3]
    }
    
  out = list(values = values, positiveDharmaOmnibus= positiveDharmaOmnibus, positiveParametric = positiveParametric , positiveDharmaNonparametric = positiveDharmaNonparametric )
  
  if(plot == T){
    plot(dispersionValues, out$positiveDharmaOmnibus, type = "b", xlab = "Overdispersion strength", ylab = "Proportion significant")
    lines(dispersionValues, out$positiveParametric, type = "b", col = "red")
    lines(dispersionValues, out$positiveDharmaNonparametric, type = "b", col = "darkgreen")
    legend("bottomright", legend = c("DHARMa Omnibus", "Parametric", "DHARMa nonparametric"), col = c("black", "red", "darkgreen"), lty = c(1,1))
    
  }
    
  return(out)
}
