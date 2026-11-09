# User-perspective test 3: typical user mistakes and very small data sets;
# for each call, check that the error or warning tells the user what to do
source("Code/DHARMaUserTests/helper.R")
set.seed(4)

d = createData(sampleSize = 100, family = poisson(), numGroups = 5, randomEffectVariance = 0)
d$Environment2 = runif(100); d$Environment2[c(3, 7)] = NA   # NA rows are dropped by the model
d$habitat = sample(c("forest", "meadow", "field"), 100, TRUE)  # character column
m = glm(observedResponse ~ Environment1 + Environment2 + habitat, family = poisson, data = d)
so = simulateResiduals(m)

# predictors of the wrong length or with NA
chk("predictor vector longer than residuals", plotResiduals(so, form = d$Environment1), expect = "error")
chk("formula predictor (handles dropped NA rows)", plotResiduals(so, form = ~Environment1))
chk("formula with all predictors", plotResiduals(so, form = ~.))
chk("unknown variable in formula", plotResiduals(so, form = ~doesNotExist), expect = "error")
chk("character predictor", plotResiduals(so, form = ~habitat))
chk("group level that does not exist", plotResiduals(so, form = ~Environment1 | habitat == "desert"), expect = "error")
chk("wrong operator in form", plotResiduals(so, form = ~Environment1 | habitat > "a"), expect = "error")
chk("testCategorical with wrong length", testCategorical(so, catPred = d$group, plot = FALSE), expect = "error")

# missing arguments
chk("testTemporalAutocorrelation without time", testTemporalAutocorrelation(so), expect = "error")
chk("testSpatialAutocorrelation without coordinates", testSpatialAutocorrelation(so), expect = "error")
chk("testCategorical without catPred", testCategorical(so), expect = "error")

# model instead of DHARMa object
chk("testDispersion(model) converts the model", testDispersion(m, plot = FALSE))
chk("recalculateResiduals(model)", recalculateResiduals(m, group = ~group), expect = "error")
chk("simulateResiduals(DHARMa object)", simulateResiduals(so), expect = "error")
chk("testDispersion(numeric residuals)", testDispersion(residuals(so)), expect = "error")

# very small data sets and few simulations
for(n in c(3, 10)){
  ds = data.frame(x = 1:n, y = rpois(n, 3))
  s = chk(paste0("n = ", n, " simulateResiduals"), simulateResiduals(glm(y ~ x, family = poisson, data = ds)))
  if(!is.null(s)) chk(paste0("n = ", n, " plot"), plot(s), png = paste0("small_n", n))
}
chk("n = 1 simulation", simulateResiduals(m, n = 1), expect = "error")

saveLog("03_userMistakes")
