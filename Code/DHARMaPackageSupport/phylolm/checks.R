# DHARMa support for phylolm: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/phylolm/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
library(phylolm)
set.seed(123)

# update() in DHARMa looks up tree and data in the global environment
tree = ape::rcoal(60)
taxa = sort(tree$tip.label)
x = rTrait(n = 1, phy = tree, model = "BM", parameters = list(ancestral.state = 0, sigma2 = 10))
y = x + rTrait(n = 1, phy = tree, model = "lambda", parameters = list(ancestral.state = 0, sigma2 = 1, lambda = 0.5))
dat = data.frame(trait = y[taxa], predictor = x[taxa], row.names = taxa)
dat$bin = rbinTrait(n = 1, phy = tree, beta = c(-1, 0.5), alpha = 1, X = cbind(1, dat$predictor))[taxa]
dat$count = rpois(60, exp(0.5 + 0.2 * dat$predictor))

# Model classes ----

models = list(
  phylolm = phylolm(trait ~ predictor, data = dat, phy = tree, model = "lambda"),
  phylolmBM = phylolm(trait ~ predictor, data = dat, phy = tree, model = "BM"),
  logistic_MPLE = phyloglm(bin ~ predictor, data = dat, phy = tree, method = "logistic_MPLE"),
  logistic_IG10 = phyloglm(bin ~ predictor, data = dat, phy = tree, method = "logistic_IG10")
)
interface = sapply(models, checkInterface, nRefit = 10)
print(t(interface))
stopifnot(sapply(models, function(m) class(m)[1]) %in% DHARMa:::getPossibleModels())
# simulations refit the model (parametric bootstrap), so phylolm may warn, e.g. about
# parameter estimates at their bounds
stopifnot(interfaceOk(interface, allowWarnings = TRUE))

# poisson_GEE ----

# simulations use update(boot = ), which phyloglm does not offer for poisson_GEE (#478)
mPois = phyloglm(count ~ predictor, data = dat, phy = tree, method = "poisson_GEE")
o = outcome(simulateResiduals(mPois))
print(o)
stopifnot(startsWith(o, "error"))

# Simulations ----

# simulations are the parametric bootstrap data sets of phylolm (update(boot = nsim, save = TRUE));
# simulateREs has no effect, the phylogenetic structure is always re-simulated
m = models$phylolm
set.seed(1); s1 = getSimulations(m, nsim = 5, simulateREs = "conditional")
set.seed(1); s2 = getSimulations(m, nsim = 5, simulateREs = "unconditional")
stopifnot(identical(s1, s2))

# Residual correlation ----

# residual rotation (see vignette, Residual correlation structures) runs for phylolm
stopifnot(!startsWith(outcome(simulateResiduals(m, n = 250, rotation = "estimated")), "error"))

# Refit ----

# getRefit() expects the new response in the order of getObservedResponse() (tree tips)
stopifnot(sapply(models, function(m) refitReproduces(m, newresp = getObservedResponse(m), fixef = coef, tolerance = 1e-3)))

# Order of residuals ----

# residuals are in the order of the tree tips, not of the data (#609)
datShuffled = dat[sample(60), ]
mShuffled = phylolm(trait ~ predictor, data = datShuffled, phy = tree, model = "lambda")
stopifnot(!inDataOrder(mShuffled, datShuffled$trait),
          inDataOrder(mShuffled, datShuffled[tree$tip.label, "trait"]))

# Pearson residuals and dispersion tests ----

res = simulateResiduals(models$logistic_MPLE)
o = c(default = outcome(testDispersion(res, plot = FALSE)),
      PearsonChisq = outcome(testDispersion(res, type = "PearsonChisq", plot = FALSE)))
print(o)
stopifnot(o["default"] == "ok", is_outcome(o["PearsonChisq"], "error", "Pearson residuals"))
