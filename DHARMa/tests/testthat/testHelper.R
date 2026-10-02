
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



test_that("hasWeights", {

  set.seed(123)

  # k/n binomial data: observedResponse1 = successes out of 20
  d = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0,
                 family = binomial(), binomialTrials = 20)
  d$prop = d$observedResponse1 / 20
  d$y01 = as.numeric(d$prop > 0.5)
  w = rep(c(1, 2), each = 100)

  # no weights
  m = glm(prop ~ Environment1, family = binomial, data = d)
  expect_false(DHARMa:::hasWeights(m))

  # proportion response + number of trials as weights: no warning (issue #540)
  m = glm(prop ~ Environment1, family = binomial, data = d, weights = rep(20, 200))
  expect_false(DHARMa:::hasWeights(m))

  # 0/1 response with prior weights
  m = glm(y01 ~ Environment1, family = binomial, data = d, weights = w)
  expect_true(DHARMa:::hasWeights(m))

  # factor response with prior weights
  d$yFactor = factor(ifelse(d$y01 == 1, "yes", "no"))
  m = glm(yFactor ~ Environment1, family = binomial, data = d, weights = w)
  expect_true(DHARMa:::hasWeights(m))

  # cbind response + weights
  m = glm(cbind(observedResponse1, observedResponse0) ~ Environment1,
          family = binomial, data = d, weights = w)
  expect_true(DHARMa:::hasWeights(m))

  # non-integer weights in binomial (glm warns about non-integer #successes)
  m = suppressWarnings(glm(prop ~ Environment1, family = binomial, data = d,
                           weights = rep(c(20, 20.5), each = 100)))
  expect_true(DHARMa:::hasWeights(m))

  # poisson with prior weights
  dp = createData(sampleSize = 200, randomEffectVariance = 0, family = poisson())
  m = glm(observedResponse ~ Environment1, family = poisson, data = dp, weights = w)
  expect_true(DHARMa:::hasWeights(m))

  # weights all equal to 1 = no weights
  m = glm(observedResponse ~ Environment1, family = poisson, data = dp,
          weights = rep(1, 200))
  expect_false(DHARMa:::hasWeights(m))
})
