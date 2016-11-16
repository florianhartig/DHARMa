library(lme4)

dataModelCreator <- function(){
  data = createData(sampleSize = 100, overdispersion = 0, family = poisson())
  model <- glmer(observedResponse ~ Environment1 + (1|group) + 
    (1|ID), data = data, family = "poisson")  
  return(list(data=data, model = model))
}

\dontrun{benchmarkUniformity(dataModelCreator = dataModelCreator, nSim = 4)}
