test_that("Conditional simulations have a smaller spread than unconditional simulations", {

  # create data
  # poisson, because required for GLMMadaptive; increased RE variance
  set.seed(123)
  testData = createData(100, randomEffectVariance = 2)

  # fit models (mgcv not included here, because simulations are always conditional for gam)
  mglmer = lme4::glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mglmmTMB = glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mspaMM = spaMM::HLfit(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mGLMMadaptive = GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 |group, data = testData, family = poisson())

  # get simulations and their SD
  glmer_cond = apply(getSimulations(mglmer, simulateREs = "conditional", nsim = 100), 2, sd)
  glmer_uncond = apply(getSimulations(mglmer, simulateREs = "unconditional", nsim = 100), 2, sd)

  glmmTMB_cond = apply(getSimulations(mglmmTMB, simulateREs = "conditional", nsim = 100), 2, sd)
  glmmTMB_uncond = apply(getSimulations(mglmmTMB, simulateREs = "unconditional", nsim = 100), 2, sd)

  spaMM_cond = apply(getSimulations(mspaMM, simulateREs = "conditional", nsim = 100), 2, sd)
  spaMM_uncond = apply(getSimulations(mspaMM, simulateREs = "unconditional", nsim = 100), 2, sd)

  GLMMadaptive_cond = apply(getSimulations(mGLMMadaptive, simulateREs = "conditional", nsim = 100), 2, sd)
  GLMMadaptive_uncond = apply(getSimulations(mGLMMadaptive, simulateREs = "unconditional", nsim = 100), 2, sd)

  # expect lower spread for conditional simulations
  expect_true(sd(glmer_cond) < sd(glmer_uncond))
  expect_true(sd(glmmTMB_cond) < sd(glmmTMB_uncond))
  expect_true(sd(spaMM_cond) < sd(spaMM_uncond))
  expect_true(sd(GLMMadaptive_cond) < sd(GLMMadaptive_uncond))

})





test_that("Unconditional predictions are the default in DHARMa", {

  # create data
  # poisson, because required for GLMMadaptive; increased RE variance
  testData = createData(100, randomEffectVariance = 2)

  # fit models
  mglmer = lme4::glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mglmmTMB = glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mspaMM = spaMM::HLfit(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mGLMMadaptive = GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 |group, data = testData, family = poisson())
  mmgcv = mgcv::gam(observedResponse ~ s(Environment1) + s(group, bs="re"), data = testData, family = poisson())

  # compare default getFitted() and unconditional predictions
  # following the package-specific syntax and using predict()
  glmer_fitted = getFitted(mglmer)
  glmer_predict = predict(mglmer, re.form = ~0, type = "response")
  expect_equal(glmer_fitted, unname(glmer_predict))

  glmmTMB_fitted = getFitted(mglmmTMB)
  glmmTMB_predict = predict(mglmmTMB, re.form = ~0, type = "response")
  expect_equal(glmmTMB_fitted, unname(glmmTMB_predict))

  spaMM_fitted = getFitted(mspaMM)
  spaMM_predict = predict(mspaMM, re.form = NA)
  expect_equal(spaMM_fitted, spaMM_predict[,1])

  GLMMadaptive_fitted = getFitted(mGLMMadaptive)
  GLMMadaptive_predict = predict(mGLMMadaptive, type = "mean_subject")
  expect_equal(GLMMadaptive_fitted, GLMMadaptive_predict)

  mmgcv_fitted = getFitted(mmgcv)
  # c() drops the 1-d array dimension of predict.gam
  mmgcv_predict = c(predict(mmgcv, type = "response", exclude = "s(group)"))
  expect_equal(mmgcv_fitted, mmgcv_predict)

})


test_that("getFitted for gam keeps offsets supplied via the offset argument", {
  set.seed(1)
  n = 200
  testData = data.frame(x = runif(n), a = runif(n, 1, 3), g = factor(sample(1:10, n, TRUE)))
  testData$y = rpois(n, testData$a * exp(1 + testData$x + rnorm(10, sd = 0.5)[testData$g]))

  # without REs, getFitted equals fitted (issue #572)
  m1 = mgcv::gam(y ~ s(x), offset = log(a), family = poisson, data = testData)
  expect_equal(unname(getFitted(m1)), unname(fitted(m1)))

  # with REs, offset as argument or in the formula gives the same unconditional predictions
  m2 = mgcv::gam(y ~ s(x) + s(g, bs = "re"), offset = log(a), family = poisson, data = testData)
  m3 = mgcv::gam(y ~ s(x) + s(g, bs = "re") + offset(log(a)), family = poisson, data = testData)
  expect_equal(getFitted(m2), getFitted(m3))
  expect_equal(getFitted(m3), c(predict(m3, type = "response", exclude = "s(g)")))

  # bam: mgcv drops offsets supplied via the offset argument from fitted values, DHARMa stops
  m4 = mgcv::bam(y ~ s(x), offset = log(a), family = poisson, data = testData)
  expect_error(simulateResiduals(m4), "offset")
  m5 = mgcv::bam(y ~ s(x) + offset(log(a)), family = poisson, data = testData)
  expect_no_error(simulateResiduals(m5, n = 10))
})


test_that("gam simulations without mgcViz work (issue #567)", {
  set.seed(1)
  testData = createData(sampleSize = 200, family = binomial(), binomialTrials = 20, randomEffectVariance = 0)
  m = mgcv::gam(cbind(observedResponse1, observedResponse0) ~ Environment1, family = binomial, data = testData)
  # named arguments in the fallback, row.names of stats:::simulate.lm repaired
  expect_equal(dim(getSimulations(m, nsim = 3, mgcViz = FALSE)), c(200, 3))
  testData$f = factor(testData$observedResponse1 > 10)
  mf = mgcv::gam(f ~ Environment1, family = binomial, data = testData)
  expect_equal(dim(getSimulations(mf, nsim = 2, mgcViz = FALSE)), c(200, 2))
  # the fallback must not use mgcViz's simulate.gam once the mgcViz namespace is loaded
  skip_if_not_installed("mgcViz")
  loadNamespace("mgcViz")
  simulationOutput = simulateResiduals(m, n = 100, mgcViz = FALSE)
  expect_equal(mean(simulationOutput$simulatedResponse), mean(simulationOutput$observedResponse), tolerance = 0.05)
})

test_that("binomial models with cbind() response and prior weights stop (issue #568)", {
  set.seed(2)
  testData = data.frame(x = rnorm(100), N = sample(5:15, 100, TRUE), g = factor(sample(1:10, 100, TRUE)), w = 2)
  testData$k = rbinom(100, testData$N, plogis(testData$x))
  msg = "cbind\\(successes, failures\\) response and additional prior weights"
  expect_error(simulateResiduals(glm(cbind(k, N - k) ~ x, family = binomial, weights = w, data = testData)), msg)
  expect_error(simulateResiduals(lme4::glmer(cbind(k, N - k) ~ x + (1|g), family = binomial, weights = w, data = testData)), msg)
  expect_error(simulateResiduals(glmmTMB::glmmTMB(cbind(k, N - k) ~ x, family = binomial, weights = w, data = testData)), msg)
  expect_error(simulateResiduals(mgcv::gam(cbind(k, N - k) ~ s(x), family = binomial, weights = w, data = testData)), msg)
  # weights = 1 and proportions with the number of trials as weights are fine
  testData$w1 = 1
  expect_no_warning(s <- simulateResiduals(glm(cbind(k, N - k) ~ x, family = binomial, weights = w1, data = testData), n = 10))
  expect_equal(dim(s$simulatedResponse), c(100, 10))
  expect_no_warning(s <- simulateResiduals(mgcv::gam(cbind(k, N - k) ~ s(x), family = binomial, weights = w1, data = testData), n = 10))
  expect_equal(mean(s$simulatedResponse), mean(s$observedResponse), tolerance = 0.1)
  expect_no_error(simulateResiduals(glm(k / N ~ x, family = binomial, weights = N, data = testData), n = 10))
})


test_that("getFittedResponse returns fitted values on the scale of the observed response", {

  set.seed(123)
  nTrials = 20
  testData = createData(200, randomEffectVariance = 0.5, family = binomial(), binomialTrials = nTrials)
  testData$prop = testData$observedResponse1 / nTrials
  w = rep(nTrials, 200)

  # k/n binomial models, number of trials via cbind(successes, failures) or via weights
  models = list(
    glm_cbind = glm(cbind(observedResponse1, observedResponse0) ~ Environment1, data = testData, family = binomial()),
    glm_weights = glm(prop ~ Environment1, weights = w, data = testData, family = binomial()),
    gam_cbind = mgcv::gam(cbind(observedResponse1, observedResponse0) ~ s(Environment1), data = testData, family = binomial()),
    gam_weights = mgcv::gam(prop ~ s(Environment1), weights = w, data = testData, family = binomial()),
    glmer_cbind = lme4::glmer(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), data = testData, family = binomial()),
    glmer_weights = lme4::glmer(prop ~ Environment1 + (1|group), weights = w, data = testData, family = binomial()),
    glmmTMB_cbind = glmmTMB::glmmTMB(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), data = testData, family = binomial()),
    glmmTMB_weights = glmmTMB::glmmTMB(prop ~ Environment1 + (1|group), weights = w, data = testData, family = binomial()),
    glmmTMB_betabinomial = glmmTMB::glmmTMB(cbind(observedResponse1, observedResponse0) ~ Environment1, data = testData, family = glmmTMB::betabinomial()),
    spaMM_cbind = spaMM::HLfit(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), data = testData, family = binomial()),
    spaMM_betabin = spaMM::fitme(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), data = testData, family = spaMM::betabin()),
    GLMMadaptive_cbind = GLMMadaptive::mixed_model(cbind(observedResponse1, observedResponse0) ~ Environment1, random = ~ 1 |group, data = testData, family = binomial()) #,
    # GLMMadaptive_betabinomial = mixed_model(fixed = cbind(observedResponse1, observedResponse0) ~ Environment1, random = ~ 1 |group, data = testData, family = beta.binomial())
  )

  for(i in names(models)){
    fittedResponse = getFittedResponse(models[[i]])
    # fitted proportions times number of trials
    expect_equal(fittedResponse, getFitted(models[[i]]) * nTrials, ignore_attr = TRUE, info = i)
    # same scale as the observed response (number of successes, not proportions). Loose tolerance, because predictions are unconditional on the random effects
    expect_equal(mean(fittedResponse), mean(getObservedResponse(models[[i]])), tolerance = 0.5, info = i)
  }

  # all other models: identical to getFitted
  models = list(
    bernoulli = glm(observedResponse1 > 10 ~ Environment1, data = testData, family = binomial()),
    poisson = glm(observedResponse1 ~ Environment1, data = testData, family = poisson()),
    gaussian = lm(observedResponse1 ~ Environment1, data = testData)
  )

  for(i in names(models)){
    expect_equal(getFittedResponse(models[[i]]), getFitted(models[[i]]), info = i)
  }
})

