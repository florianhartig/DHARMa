#' Overdispersion benchmarks
#' 
#' This function runs Power / Type I error simulations for overdispersion tests in DHARMa
#' 
#' @param overdispersion amount of overdispersion
#' @param nRep replicates
#' @param alpha significance level
#' @param plot whether to do a plot
#' @param parallel whether to use parallel computations. Possible values are F, T (parallel cores set to number of cores in the computer -1), or an integer number for the number of cores that should be used
#' @param ... parameters to pass on to \code{\link{simulateResiduals}}
#' @details This function runs Type I error / power comparisons for overdispersion tests in DHARMA. Compared are a) the omnibus test \code{\link{testUniformity}} b) the parameteric dispersion test \code{\link{testOverdispersionParametric}}, and the nonparametric dispersion test \code{\link{testOverdispersion}}.
#' @seealso \code{\link{benchmarkUniformity}}
#' @note The benchmark function in DHARMa are intended for development purposes, and for users that want to test / confirm the properties of functions in DHARMa. If you are running an applied data analysis, they are probably of little use. 
#' @export 
benchmarkOverdispersion <- function(dispersionValues = 0, nRep = 10, alpha = 0.05, plot = T, parallel = F, ...){
  
    library(lme4)
  
    values = list()

    positiveDharmaOmnibus= numeric(length(dispersionValues))
    positiveParametric= numeric(length(dispersionValues))
    positiveDharmaNonparametric = numeric(length(dispersionValues))
    
    for(j in 1:length(dispersionValues)){
      
      getP <- function(...){
        testData = createData(sampleSize = 250, fixedEffects = c(1,0.2,0.1,0.02), quadraticFixedEffects = c(0.1,-0.4,0.2,-0.1), overdispersion = dispersionValues[j], family = poisson(), ...)
        
        fittedModel <- glmer(observedResponse ~ Environment1 + I(Environment1^2) + Environment2 + I(Environment2^2) + Environment3 + I(Environment3^2) + Environment4 + I(Environment4^2) + (1|group) , family = "poisson", data = testData)
        
        
        simulationOutput <- simulateResiduals(fittedModel = fittedModel, ...)
        out = rep(NA,3)
        x = testUniformity(simulationOutput)
        out[1] = x$p.value    
        
        out[2] = testOverdispersionParametric(model = fittedModel)$p.value
        
        simulationOutput <- simulateResiduals(fittedModel = simulationOutput$fittedModel, refit = T, ...)
        x = testOverdispersion(simulationOutput, plot = F)
        out[3] = x$p.value
        return(out)
      }
      
        
      if (parallel == F){
        out = replicate(nRep, getP(), simplify = "array", ...)
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
        
        out <- foreach::foreach(i=1:nRep, .packages=c("lme4", "DHARMa") , .combine = rbind) %dopar% getP(...)
        
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
