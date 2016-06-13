#' creates Poisson data overdispersion and random intercept
createPoissonData <- function(replicates=1, sampleSize = 2000, intercept = 0, slope = 1, numGroups = 10, randomEffectVariance = 1, overdispersion = 0.5){
  out = list()
  for (i in 1:replicates){
    environment1 = seq(-1,1,len = sampleSize)
    group = rep(1:numGroups, each = sampleSize/numGroups)
    groupRandom = rnorm(numGroups, sd = randomEffectVariance)
    counts = rpois(sampleSize, exp(slope * environment1 + intercept + groupRandom[group] + rnorm(sampleSize, sd = overdispersion)))
    out[[i]] <- data.frame(ID = 1:2000, counts, environment1, group)
  }
  return(out)
}
