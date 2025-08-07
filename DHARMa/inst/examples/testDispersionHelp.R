library(lme4)
set.seed(123)

testData = createData(sampleSize = 100, overdispersion = 0.5, randomEffectVariance = 1)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group),
                     family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# default DHARMa dispersion test using simulations conditional on fitted REs (more powerful for mixed models)
testDispersion(simulationOutput)
testDispersion(simulationOutput, alternative = "less", plot = FALSE) # only underdispersion
testDispersion(simulationOutput, alternative = "greater", plot = FALSE) # only overdispersion

# DHARMa also implements the popular Pearson-chisq test that is also on the glmmWiki by Ben Bolker
# The issue with this test is that it requires the df of the model, which are not well defined
# for GLMMs. It is biased towards underdispersion, with bias getting larger with the number
# of RE groups. In doubt, only test for overdispersion
testDispersion(simulationOutput, type = "PearsonChisq", alternative = "greater")

# if refit = TRUE, a different test on simulated Pearson residuals will calculated (see help)
simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = TRUE, seed = 12, n = 20)
testDispersion(simulationOutput2)

# often useful to test dispersion per group (in particular for binomial data, see vignette)
simulationOutputAggregated = recalculateResiduals(simulationOutput2, group = testData$group)
testDispersion(simulationOutputAggregated)




