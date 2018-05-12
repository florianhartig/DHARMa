#' Benchmark calculations
#' 
#' This function runs statistical benchmarks, including Power / Type I error simulations for an arbitrary test with a control parameter
#' 
#' @param controlValues a vector with a control parameter (e.g. to vary the strength of a problem the test should be specific to)
#' @param calculateStatistics the statistics to be benchmarked. Should return one value, or a vector of values. If controlValues are given, must accept a paramteter control
#' @param nRep number of replicates per level of the controlValues
#' @param alpha significance level
#' @param parallel whether to use parallel computations. Possible values are F, T (sets the cores automatically to number of available cores -1), or an integer number for the number of cores that should be used for the cluster
#' @param ... additional parameters to calculateStatistics 
#' @note The benchmark function in DHARMa are intended for development purposes, and for users that want to test / confirm the properties of functions in DHARMa. If you are running an applied data analysis, they are probably of little use. 
#' @export 
#' @importFrom foreach "%dopar%"
runBenchmarks <- function(calculateStatistics, controlValues = NULL, nRep = 10, alpha = 0.05, parallel = F, ...){
  

  # Sequential Simulations
  
  simulations = list()
  
  if(parallel == F){
    if(is.null(controlValues)) simulations[[1]] = replicate(nRep, calculateStatistics(), simplify = "array")
    else for(j in 1:length(controlValues)){
      simulations[[j]] = replicate(nRep, calculateStatistics(controlValues[j]), simplify = "array")
    }

  # Parallel Simulations
    
  }else{
    
    requireNamespace(foreach)
    
    if (parallel == T | parallel == "auto"){
      cores <- parallel::detectCores() - 1
      message("parallel, set cores automatically to ", cores)
    } else if (is.numeric(parallel)){
      cores <- parallel
      message("parallel, set number of cores by hand to ", cores)
    } else stop("wrong argument to parallel")
    
    cl <- parallel::makeCluster(cores)

    doParallel::registerDoParallel(cl)
    
    if(is.null(controlValues)) simulations[[1]] =  foreach::foreach(i=1:nRep, .packages=c("lme4", "DHARMa"), .combine = rbind) %dopar% calculateStatistics() 

    else for(j in 1:length(controlValues)){
      simulations[[j]] = foreach::foreach(i=1:nRep, .packages=c("lme4", "DHARMa"), .combine = rbind) %dopar% calculateStatistics(controlValues[j])
    }

    # parallel::clusterExport(cl=cl,varlist = c("calculateStatistics"), envir=environment())
    # parallel::clusterExport(cl=cl,varlist = c("controlValues", "alpha", envir=environment())
    #if(is.null(controlValues)) simulations[[1]] =  parallel::parSapply(cl, 1:nRep, calculateStatistics())
    #else for(j in 1:length(controlValues)){
    #  simulations[[j]] = parallel::parSapply(cl, 1:nRep, calculateStatistics(controlValues[j]))
    #}
    
    
    parallel::stopCluster(cl)
  }
  
  # Calculations of summaries
  
  if(is.null(controlValues)) controlValues = c("N")
  
  nOutputs = nrow(simulations[[1]])
  nControl = length(controlValues)

  summary = array(dim = c(nOutputs, 3, nControl))
  
  dimnames(summary) = list(1:nOutputs, c("sign", "mean", "unif"), controlValues)
  
  sig <- function(x) mean(x < alpha)
  isUnif <- function(x) ks.test(x, "punif")$p.value
  
  
  for (i in 1:nControl){
    summary[,1,i] = apply(simulations[[i]], 1, sig) 
    summary[,2,i] = apply(simulations[[i]], 1, mean) 
    summary[,3,i] = suppressWarnings(apply(simulations[[i]], 1, isUnif))
  }
  
  out = list()
  out$simulations = simulations
  out$summaries = summary

# 
#   if(plot == T){
#     plot(controlValues, positive, type = "b", xlab = "Control", ylab = "Proportion significant", ylim = c(0,1))
#     abline(h=alpha)
#   }
  
  return(out)
}


#' Plot distribution of p-values
#' @param x vector of p values
#' @param plot should the values be plottet
#' @param main title for the plot
#' @param ... additional arguments to hist
#' @author Florian Hartig
testPDistribution <- function(x, plot = T, main = "p distribution \n expected is flat at 1", ...){
  out = suppressWarnings(ks.test(x, 'punif'))
  hist(x, xlim = c(0,1), breaks = 20, freq = F, main = main, ...)
  abline(h=1, col = "red")
  return(out)
}


# if(plot == T){
#   oldpar <- par(mfrow = c(4,4))
#   hist(out, breaks = 50, col = "red", main = paste("mean of", nSim, "simulations"))
#   for (i in 1:min(nSim, 15)) hist(out[i,], breaks = 50, freq = F, main = i)
#   par(oldpar)    
# }



generateGenerator <- function(mod){
  
  out <- function(){
    
    simulations = simulate(mod, nsim = 1)
    
    newData <-model.frame(mod)  
    
    if(is.vector(simulations[[1]])){
      newData[,1] = simulations[[1]]
    } else {
      # Hack to make the binomial n/k case work
      newData[[1]] = NULL
      newData = cbind(simulations[[1]], newData)
    }
    
    refittedModel = update(mod, data = newData)
    
    list(data = newData, model = refittedModel)
    
  }
}

