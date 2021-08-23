#' Benchmark calculations
#' 
#' This function runs statistical benchmarks, including Power / Type I error simulations for an arbitrary test with a control parameter
#' 
#' @param controlValues optionally, a vector with a control parameter (e.g. to vary the strength of a problem the test should be specific to). See help for an example
#' @param calculateStatistics the statistics to be benchmarked. Should return one value, or a vector of values. If controlValues are given, must accept a parameter control
#' @param nRep number of replicates per level of the controlValues
#' @param alpha significance level
#' @param parallel whether to use parallel computations. Possible values are F, T (sets the cores automatically to number of available cores -1), or an integer number for the number of cores that should be used for the cluster
#' @param ... additional parameters to calculateStatistics 
#' @note The benchmark function in DHARMa are intended for development purposes, and for users that want to test / confirm the properties of functions in DHARMa. If you are running an applied data analysis, they are probably of little use. 
#' @return A object with list structure of class DHARMaBenchmark. Contains an entry simulations with a matrix of simulations, and an entry summaries with an list of summaries (significant (T/F), mean, p-value for KS-test uniformity). Can be plotted with \code{\link{plot.DHARMaBenchmark}}
#' @export 
#' @importFrom foreach "%dopar%"
#' @author Florian Hartig
#' @seealso \code{\link{plot.DHARMaBenchmark}}
#' @example inst/examples/runBenchmarksHelp.R
runBenchmarks <- function(calculateStatistics, controlValues = NULL, nRep = 10, alpha = 0.05, parallel = FALSE, ...){
  

  start_time <- Sys.time()
  
  # Sequential Simulations
  
  simulations = list()
  
  if(parallel == FALSE){
    if(is.null(controlValues)) simulations[[1]] = replicate(nRep, calculateStatistics(), simplify = "array")
    else for(j in 1:length(controlValues)){
      simulations[[j]] = replicate(nRep, calculateStatistics(controlValues[j]), simplify = "array")
    }

  # Parallel Simulations
    
  }else{
    
    if (parallel == TRUE | parallel == "auto"){
      cores <- parallel::detectCores() - 1
      message("parallel, set cores automatically to ", cores)
    } else if (is.numeric(parallel)){
      cores <- parallel
      message("parallel, set number of cores by hand to ", cores)
    } else stop("wrong argument to parallel")
    
    cl <- parallel::makeCluster(cores)

    # for each
    # doParallel::registerDoParallel(cl)
    # 
    # `%dopar%` <- foreach::`%dopar%`
    # 
    # if(is.null(controlValues)) simulations[[1]] =  t(foreach::foreach(i=1:nRep, .packages=c("lme4", "DHARMa"), .combine = rbind) %dopar% calculateStatistics())
    # 
    # else for(j in 1:length(controlValues)){
    #   simulations[[j]] = t(foreach::foreach(i=1:nRep, .packages=c("lme4", "DHARMa"), .combine = rbind) %dopar% calculateStatistics(controlValues[j]))
    # }
    # 
    # End for each

    # doesn't see to work properly
    loadedPackages = (.packages())
    parExectuer = function(x = NULL, control = NULL) calculateStatistics(control)
    parallel::clusterExport(cl = cl, c("parExectuer", "loadedPackages"), envir = environment())
    parallel::clusterEvalQ(cl, {for(p in loadedPackages) library(p, character.only=TRUE)})
    
    # parallel::clusterExport(cl = cl, varlist = ls(envir = .GlobalEnv))
    
    # parallel::clusterExport(cl=cl,varlist = c("calculateStatistics"), envir=environment())
    # parallel::clusterExport(cl=cl,varlist = c("controlValues", "alpha", envir=environment())
    
    # parallel::clusterExport(cl=cl,varlist = c("calculateStatistics"), envir=environment())
    # parallel::clusterExport(cl=cl,varlist = c("controlValues", "alpha", envir=environment())
    
    if(is.null(controlValues)) simulations[[1]] = parallel::parSapply(cl, 1:nRep, parExectuer)
    else for(j in 1:length(controlValues)){
      simulations[[j]] = parallel::parSapply(cl, 1:nRep, parExectuer, control = controlValues[j])
    }
    
    parallel::stopCluster(cl)
  }
  
  # Calculations of summaries
  
  if(is.null(controlValues)) controlValues = c("N")
  
  nOutputs = nrow(simulations[[1]])
  nControl = length(controlValues)
  
  # reducing the list of outputs to a data.frame
  x = Reduce(rbind, lapply(simulations, t))
  x = data.frame(x)
  x$replicate = rep(1:nRep, length(controlValues))
  x$controlValues = rep(controlValues, each = nRep)

  summary = list()
  
  # function for aggregation
  aggreg <- function(f) {
    ret <- aggregate(x[,- c(ncol(x) - 1, ncol(x))], by = list(x$controlValues), f)
    colnames(ret)[1] = "controlValues"
    return(ret)
  }

  sig <- function(x) mean(x < alpha)
  isUnif <- function(x) ks.test(x, "punif")$p.value
  
  summary$propSignificant = aggreg(sig)
  summary$meanP = aggreg(mean)
  summary$isUnifP = aggreg(mean)

  out = list()
  out$controlValues = controlValues
  out$simulations = x
  out$summaries = summary
  out$time = Sys.time() - start_time
  out$nSummaries = ncol(x) - 2
  out$nReplicates = nRep
  
  class(out) = "DHARMaBenchmark"
  
  return(out)
}


#' Plots DHARMa benchmarks
#' 
#' The function plots the result of an object of class DHARMaBenchmark, created by \code{\link{runBenchmarks}}
#' 
#' @param x object of class DHARMaBenchmark, created by \code{\link{runBenchmarks}}
#' @param ... parameters to pass to the plot function
#' 
#' @details The function will create two types of plots, depending on whether the run contains only a single value (or no value) of the control parameter, or whether a vector of control values was provided.
#' 
#' If a single or no value of the control parameter was provided, the function will create box plots of the estimated p-values, with the number of significant p-values plotted to the left
#' 
#' If a control parameter was provided, the function will plot the proportion of significant p-values against the control parameter, with 95% CIs based based on the performed replicates displayed as confidence bands
#' 
#' @seealso \code{\link{runBenchmarks}}
#' @export
plot.DHARMaBenchmark <- function(x, ...){
  
  if(length(x$controlValues)== 1){
    boxplot(x$simulations[,1:x$nSummaries], col = "grey", ylim = c(-0.3,1), horizontal = T, las = 2,  xaxt='n', main = "p distribution", ...)
    abline(v = 0)
    abline(v = c(0.25, 0.5, 0.75), lty = 2)
    text(-0.2, 1:x$nSummaries, labels = x$summaries$propSignificant[-1])
    # barplot(as.matrix(x$summaries$propSignificant[-1]), horiz = T, add = T, offset = -0.2, names.arg = "test", width = 0.5, space = 1.4)
  }else{
    res = x$summaries$propSignificant
    
    plot(NULL, xlim = range(res$controlValues), ylim = c(0,1), ...)
    for(i in 1:x$nSummaries){
      
      getCI = function(k) as.vector(binom.test(k,x$nReplicates)$conf.int)
      CIs = sapply(res[,i+1]*x$nReplicates, getCI)
      
      polygon(c(res$controlValues, rev(res$controlValues)),
              c(CIs[1,], rev(CIs[2,])),
              col = "#00000020", border = F)
      lines(res$controlValues, res[,i+1], col = i, lty = i, lwd = 2)
    }
    legend("bottomright", colnames(res[,-1]), col = 1:x$nSummaries, lty = 1:x$nSummaries, lwd = 2)    
    
  }
}

# this used to be an alternative to the boxplot for control = N

plotMultipleHist <- function(x){
  
  lin = ncol(x)
  histList <- lapply(x, hist, breaks = seq(0,1,0.02), plot = F)
  
  plot(NULL, xlim = c(0,1), ylim = c(0, lin), yaxt = 'n', ylab = NULL, xlab = "p-value")
  abline(h= 0)
  
  for(i in 1:lin){
    maxD = max(histList[[i]]$density)
    
    lines(histList[[i]]$mids, i - 1 + histList[[i]]$density/maxD, type = "l")
    abline(h= i)
    abline(h= 1/maxD + i - 1, , col = "red", lty = 2)
  } 
  
  abline(v = 1, lty = 2)
  abline(v = c(0.05, 0), lty = 2, col = "red")
}






#' Plot distribution of p-values
#' @param x vector of p values
#' @param plot should the values be plotted
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

