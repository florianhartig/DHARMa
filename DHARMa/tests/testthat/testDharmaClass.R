
test_that("createDHARMa works", {

  testData = createData(sampleSize = 200, family = poisson(),
                        randomEffectVariance = 0, numGroups = 5)
  fittedModel <- glm(observedResponse ~ Environment1,
                     family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  truth = testData$observedResponse
  pred = simulationOutput$fittedPredictedResponse
  simulatedResponse = simulationOutput$simulatedResponse

  sim = createDHARMa(simulatedResponse = simulatedResponse,
                     observedResponse = truth,
                     fittedPredictedResponse = pred,
                     integerResponse = T)
  expect_match(class(sim), "DHARMa")

  expect_message(sim2 <- createDHARMa(simulatedResponse = simulatedResponse,
                              observedResponse = truth), "No fitted predicted")
  expect_match(class(sim2), "DHARMa")


})


test_that("recalcuateResiduals works", {

  testData = createData(sampleSize = 200, family = poisson(), numGroups = 20,
                        randomEffectVariance = 1)

  fittedModel <- glm(observedResponse ~ Environment1, family = "poisson",
                     data = testData)

  simulationOutput = simulateResiduals(fittedModel)

  ##### only grouping #####

  simulationOutput2 = recalculateResiduals(simulationOutput,
                                           group = testData$group)
  expect_true(length(residuals(simulationOutput2)) == simulationOutput2$nGroups)


  ##### only selection #####

  #error
  expect_error(recalculateResiduals(simulationOutput, sel = 1))

  #more than nObs in selection
  simulationOutput3 = recalculateResiduals(simulationOutput, sel = 1:400)
  expect_true(length(residuals(simulationOutput3))==simulationOutput3$nObs)

  #less than nObs in selection
  expect_true(length(residuals(recalculateResiduals(simulationOutput,
                                                    sel = 1:2))) == 2)

  expect_true(length(residuals(recalculateResiduals(simulationOutput,
                                                    sel = c(1,6,8)))) == 3)

  expect_true(length(residuals(recalculateResiduals(
    simulationOutput, sel = testData$group == 1))) == 10)

  ##### selection and grouping #####
  # selection by different variables
  simulationOutput4 = recalculateResiduals(simulationOutput,
                                           sel = testData$group)
  expect_true(length(residuals(simulationOutput4)) ==
                nlevels(simulationOutput4$sel))


  #selecting = float   -   wrong dimensions (Sel has length 0)
  expect_true(length(residuals(recalculateResiduals(
    simulationOutput, sel = 1.0:20.0))) == 20)
  #just one group within the selection
  expect_error(recalculateResiduals(simulationOutput, group = testData$group,
                                    sel = 1.0:10.0))


  expect_true(length(residuals(recalculateResiduals(
    simulationOutput, sel = 1.5 :10.7))) == 10) # not checking float
  # simulationOutput2 = recalculateResiduals(simulationOutput, group = testData$group, sel = 1.5:10.6)    # not working, don´t has to work


  # selecting = negative Values
  expect_error(length(residuals(recalculateResiduals(
    simulationOutput, sel = -10 : 5))))
  expect_true(length(residuals(recalculateResiduals(simulationOutput,
                                                    sel = -10:-5))) == 194)

  # checking NULL functions
  simulationOutput3 = recalculateResiduals(simulationOutput, sel =  NULL)
  expect_true(length(residuals(simulationOutput3)) ==
                simulationOutput3$nObs)
  simulationOutput3 = recalculateResiduals(simulationOutput, group = NULL,
                                           sel = NULL)
  expect_true(length(residuals(simulationOutput3)) ==
                simulationOutput3$nObs)
  simulationOutput3 = recalculateResiduals(simulationOutput, group = NULL,
                                           sel = c(1:5,8,9), method = "PIT")

  simulationOutput3 = recalculateResiduals(simulationOutput, group = NULL,
                                           sel = c(1:5,8,9),
                                           method = "traditional")
  expect_true(length(residuals(simulationOutput3)) ==
                length(simulationOutput3$sel))

  # checking for
  expect_no_error(recalculateResiduals(simulationOutput,
                                           group = 1:simulationOutput$nObs,
                      sel = simulationOutput$nObs/2:simulationOutput$nObs,
                      method = "traditional"))

  expect_no_error(recalculateResiduals(simulationOutput,
                                           group = 1:simulationOutput$nObs,
                      sel = simulationOutput$nObs/3.5:simulationOutput$nObs,
                      method = "PIT"))

})



