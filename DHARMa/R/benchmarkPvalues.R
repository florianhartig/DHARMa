#' General Type I/II benchmarks
#' 
#' This function runs Power / Type I error simulations for an arbitrary test with a control parameter
#' 
#' @param controlValues a vector with a control parameter (e.g. to vary the strength of a problem the test should be specific to)
#' @param getP the test to be benchmarked - this should be a function that takes one of the controlValues as an input, and returns a p-value
#' @param nRep number of replicates per level of the controlValues
#' @param alpha significance level
#' @param plot whether to do a plot
#' @param parallel whether to use parallel computations. Possible values are F, T (parallel cores set to number of cores in the computer -1), or an integer number for the number of cores that should be used
#' @seealso \code{\link{benchmarkUniformity}}
#' @note The benchmark function in DHARMa are intended for development purposes, and for users that want to test / confirm the properties of functions in DHARMa. If you are running an applied data analysis, they are probably of little use. 
#' @export 
#' @importFrom foreach "%dopar%"
benchmarkP <- function(controlValues = 0, getP, nRep = 10, alpha = 0.05, plot = T, parallel = F, ...){
  
  values = list()
  
  positive = numeric(length(controlValues))

  for(j in 1:length(controlValues)){
    
    if (parallel == F){
      out = replicate(nRep, getP(controlValues[j]), simplify = "array", ...)
      out = t(out)
    }else{
      if (parallel == T | parallel == "auto"){
        cores <- parallel::detectCores() - 1
        message("parallel, set cores automatically to ", cores)
      } else if (is.numeric(parallel)){
        cores <- parallel
        message("parallel, set number of cores by hand to ", cores)
      } else stop("wrong argument to parallel")
      
      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      out <- foreach::foreach(i=1:nRep, .packages=c("lme4", "DHARMa"), .combine = rbind) %dopar% getP(controlValues[j]) 
      parallel::stopCluster(cl = cl)
    }
    
    sig <- function(x) mean(x < alpha)
    
    means = colMeans(out)
    significant = apply(out, 2, sig)
    values[[j]] = list(pValues = out, means = means)

  }

  if(plot == T){
    plot(controlValues, positive, type = "b", xlab = "Control", ylab = "Proportion significant", ylim = c(0,1))
    abline(h=alpha)
  }
  
  return(positive)
}
