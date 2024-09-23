
test_that("ensureDHARMa", {

  set.seed(123)

  testData = createData(sampleSize = 200, overdispersion = 3, pZeroInflation = 0.4, randomEffectVariance = 0)

  pred = testData$Environment1

  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)

  expect_error(getSimulations(fittedModel, 1, type = "refdt"))

  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  expect_s3_class(DHARMa:::ensureDHARMa(simulationOutput), "DHARMa")
  expect_error(DHARMa:::ensureDHARMa(simulationOutput$scaledResiduals), "DHARMa")
  expect_error(DHARMa:::ensureDHARMa(fittedModel), "DHARMa")

  expect_s3_class(DHARMa:::ensureDHARMa(simulationOutput, convert = T), "DHARMa")
  expect_s3_class(DHARMa:::ensureDHARMa(simulationOutput$scaledResiduals, convert = T), "DHARMa")
  expect_s3_class(DHARMa:::ensureDHARMa(fittedModel, convert = T), "DHARMa")
  expect_error(DHARMa:::ensureDHARMa(matrix(rnorm(100), nrow = 4), convert = T))
  expect_error(DHARMa:::ensureDHARMa(list(c = 1), convert = T))

  expect_s3_class(DHARMa:::ensureDHARMa(fittedModel, convert = "Model"), "DHARMa")
  expect_error(DHARMa:::ensureDHARMa(simulationOutput$scaledResiduals, convert = "Model"))
  expect_error(DHARMa:::ensureDHARMa(matrix(rnorm(100))), "DHARMa")



  DHARMa:::ensurePredictor(simulationOutput, predictor = pred)
  DHARMa:::ensurePredictor(simulationOutput)
  DHARMa:::ensurePredictor(simulationOutput, predictor = testData$observedResponse)
  expect_error(DHARMa:::ensurePredictor(simulationOutput, predictor = c(1,2,3)))




  # testResiduals tests distribution, dispersion and outliers
  expect_error(testQuantiles(simulationOutput$scaledResiduals))

})



test_that("randomSeed", {

  runif(1)
  # testing the function in standard settings
  currentSeed = .Random.seed
  x = getRandomState(123)
  runif(1)
  x$restoreCurrent()
  expect_true(all(.Random.seed == currentSeed))

  # if no seed was set in env, this will also be restored

  rm(.Random.seed, envir = globalenv()) # now, there is no random seed
  x = getRandomState(123)
  expect_true(exists(".Random.seed"))  # TRUE
  runif(1)
  x$restoreCurrent()
  expect_false(exists(".Random.seed")) # False
  runif(1) # re-create a seed

  # with seed = false
  currentSeed = .Random.seed
  x = getRandomState(FALSE)
  runif(1)
  x$restoreCurrent()
  expect_false(all(.Random.seed == currentSeed))

  # with seed = NULL
  currentSeed = .Random.seed
  x = getRandomState(NULL)
  runif(1)
  x$restoreCurrent()
  expect_true(all(.Random.seed == currentSeed))

})
