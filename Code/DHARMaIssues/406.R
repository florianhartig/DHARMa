## improving the information about errors in qgam package to calculate quantile regression of the residuals
# from issue #406

library(DHARMa)
library(glmmTMB)

# reproducible example from the issue
habitat <- c(rep("Town", 6), rep("Country", 6), rep("City", 6))
location <- c(rep(c("a","b","c"), 6))
beetles <- c(275, 89, 776, 325, 3302, 3335, 288, 566, 1546, 80, 282, 2529, 550, 479, 983, 372, 450, 902)
testData <- data.frame(habitat, location, beetles)

# it doesn't throw an error with 18 data points
M2_Total <- glmmTMB(beetles ~ habitat + (1 | location),
                    family = "nbinom2", data=testData)
simulationOutput_M2_Total <- simulateResiduals(fittedModel = M2_Total,  plot = F)
plot(simulationOutput_M2_Total)

# with 14 data points, we can see the warning messages, but quantiles are plotted:
M3_Total <- glmmTMB(beetles ~ habitat + (1 | location),
                    family = "nbinom2", data=testData[1:14,])
simulationOutput_M3_Total <- simulateResiduals(fittedModel = M3_Total,  plot = F)
plot(simulationOutput_M3_Total)

# with 13 data points, we can see the error messages + warnings, just ONE quantile is plotted:
M4_Total <- glmmTMB(beetles ~ habitat + (1 | location),
                    family = "nbinom2", data=testData[1:13,])
simulationOutput_M4_Total <- simulateResiduals(fittedModel = M4_Total,  plot = F)
plot(simulationOutput_M4_Total)

# with 10 data points, we can see the error messages + warnings, no quantiles plotted:
M5_Total <- glmmTMB(beetles ~ habitat + (1 | location),
                    family = "nbinom2", data=testData[1:10,])
simulationOutput_M5_Total <- simulateResiduals(fittedModel = M5_Total,  plot = F)
plot(simulationOutput_M5_Total)

