testData = createData(sampleSize = 200, family = poisson(),
                      fixedEffects = c(1,1),
                      randomEffectVariance = 1, numGroups = 10)
testData$Environment2[1] = NA
fittedModel <- glm(observedResponse ~ Environment1 + Environment2,
                   family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

#############  residual plots  ###############

# Default in DHARMa is to show predictions rank transformed
# if you want plots based on raw predictions, use rank = F
plotResiduals(simulationOutput, rank = FALSE)

# smooth scatter plot - default for large datasets with n > 10,000
plotResiduals(simulationOutput, rank = TRUE, smoothScatter = TRUE)

\dontrun{

  # It is very advisable to plot the residual against all predictors
  # the following syntax uses the predictor values from the fitted model
  plotResiduals(simulationOutput, form = ~ Environment1)


  # plot against all predictors
  plotResiduals(simulationOutput, form = ~.)

  # if pred is a factor, or if asFactor = TRUE, will produce a boxplot
  plotResiduals(simulationOutput, form = ~group)

  # plot residuals against multiple predictors at once
  plotResiduals(simulationOutput, form = ~Environment1 + Environment2)

  # plot residuals against a predictor for all respective group levels
  plotResiduals(simulationOutput, form = ~Environment1|group)

  # plot residuals against a predictor for a specific group level, here group 1
  plotResiduals(simulationOutput, form = ~Environment1|group == "1")





  # alternatively, you can plot against a variable from the global environment
  # in this case, the model function automatically removed one row of observations
  # because there was an NA in Environment2. When using the variable Environment1
  # from the global environment, we have to remove this observation as well
  plotResiduals(simulationOutput,
                form = testData$Environment1[complete.cases(testData)])



  # to diagnose overdispersion and heteroskedasticity it can be useful to
  # display residuals as absolute deviation from the expected mean 0.5
  plotResiduals(simulationOutput, absoluteDeviation = TRUE)

  # All these options can also be provided to the main plotting function

  # If you want to plot summaries per group, use recalculateResiduals
  # for details see help of recalculateResiduals
}
