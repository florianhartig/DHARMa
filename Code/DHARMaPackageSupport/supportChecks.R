# Generic checks for DHARMa package support
#
# Functions used by the checks.R scripts in the package folders. Run from the
# repository root; the scripts source this file, which loads the current DHARMa
# (devtools::load_all) unless DHARMa is already loaded.
#
# Successor of the former testModel() (archive/testCompatibility.R) and of
# archive/comparingPackages_condUnconditional_GLMM.R.

if (!"DHARMa" %in% loadedNamespaces()) devtools::load_all("DHARMa", quiet = TRUE)

# Outcome of an expression ----

# "ok", the error raised by expr, or the warnings and messages raised by expr,
# separated by " | ", e.g. "warning: Model was fit with prior weights. ..."
outcome <- function(expr) {
  cond = NULL
  keep = function(type) function(c) {
    cond <<- c(cond, paste0(type, ": ", trimws(conditionMessage(c))))
    invokeRestart(paste0("muffle", tools::toTitleCase(type)))
  }
  res = withCallingHandlers(
    tryCatch({force(expr); "ok"}, error = function(e) paste0("error: ", conditionMessage(e))),
    warning = keep("warning"), message = keep("message"))
  if (res == "ok" && !is.null(cond)) paste(cond, collapse = " | ") else res
}

# TRUE if outcome() starts with type ("ok", "warning", "message", "error") and,
# if given, contains pattern (in any of the conditions)
is_outcome <- function(x, type, pattern = NULL) {
  startsWith(x, type) && (is.null(pattern) || grepl(pattern, x, fixed = TRUE))
}

# Standard interface ----

# Runs the DHARMa wrappers and simulateResiduals() on a fitted model and checks
# their output format, as runEverything() in DHARMa/tests/testthat/testModelTypes.R.
# Returns a named character vector of outcomes.
checkInterface <- function(model, refit = TRUE, nRefit = 20) {
  nObs = tryCatch(length(getObservedResponse(model)), error = function(e) NA)
  out = c(
    observedResponse = outcome({
      x = getObservedResponse(model)
      stopifnot(is.vector(x), is.numeric(x))
    }),
    simulations = outcome({
      x = getSimulations(model, nsim = 2)
      stopifnot(is.matrix(x), is.numeric(x), dim(x) == c(nObs, 2))
    }),
    simulationsRefit = outcome(stopifnot(is.data.frame(getSimulations(model, nsim = 2, type = "refit")))),
    fitted = outcome(stopifnot(length(getFitted(model)) == nObs)),
    fittedResponse = outcome(stopifnot(length(getFittedResponse(model)) == nObs)),
    simulateResiduals = outcome({
      res = simulateResiduals(model, n = 100)
      stopifnot(length(res$scaledResiduals) == nObs, !anyNA(res$scaledResiduals))
    })
  )
  if (refit) out["simulateResidualsRefit"] = outcome({
    res = simulateResiduals(model, n = nRefit, refit = TRUE)
    stopifnot(length(res$scaledResiduals) == nObs, !anyNA(res$scaledResiduals))
  })
  out
}

# TRUE if all outcomes of checkInterface() are "ok", except that refits may raise
# warnings of the fitting package (e.g. convergence warnings for some simulated data sets).
# With allowWarnings = TRUE, all checks may raise warnings (for packages that refit the
# model to simulate, e.g. phylolm).
interfaceOk <- function(interface, allowWarnings = FALSE) {
  interface = as.matrix(interface)
  isRefit = rownames(interface) == "simulateResidualsRefit" | allowWarnings
  all(interface[!isRefit, ] == "ok") && !any(startsWith(interface[isRefit, ], "error"))
}

# Refit ----

# TRUE if refitting the model to its own observed response reproduces the fixed effects
refitReproduces <- function(model, newresp = model.response(model.frame(model)),
                            fixef = getFixedEffects, tolerance = 1e-4) {
  refitted = getRefit(model, newresp)
  isTRUE(all.equal(unlist(fixef(refitted)), unlist(fixef(model)), tolerance = tolerance, check.attributes = FALSE))
}

# Conditional vs. unconditional simulations ----

# Mean variance of the simulated values per observation, for each simulateREs
# option. Conditional simulations should have a smaller variance than
# unconditional ones if the model has random effects.
simulationVariance <- function(model, nsim = 200, options = c("conditional", "unconditional")) {
  sapply(options, function(o) {
    sims = suppressWarnings(suppressMessages(getSimulations(model, nsim = nsim, simulateREs = o)))
    mean(apply(sims, 1, var))
  })
}

# Default simulations ----

# TRUE if simulateResiduals() without simulateREs gives the same simulations as
# simulateREs = "conditional"
defaultIsConditional <- function(model, n = 50) {
  set.seed(1); s1 = simulateResiduals(model, n = n)
  set.seed(1); s2 = simulateResiduals(model, n = n, simulateREs = "conditional")
  identical(s1$simulatedResponse, s2$simulatedResponse)
}

# Dispersion tests ----

# outcomes of testDispersion() with the default test, the Pearson test, and after
# refit = TRUE (which also uses Pearson residuals). The Pearson test gives a
# message for mixed models (biased towards underdispersion).
dispersionTests <- function(model) {
  res = simulateResiduals(model)
  c(default = outcome(testDispersion(res, plot = FALSE)),
    PearsonChisq = outcome(testDispersion(res, type = "PearsonChisq", plot = FALSE)),
    refit = outcome(testDispersion(simulateResiduals(model, refit = TRUE, n = 20), plot = FALSE)))
}

# Warnings ----

# evaluates expr and stops if it raises a warning (e.g. to check that model fits converge)
noWarnings <- function(expr) withCallingHandlers(expr, warning = function(w) stop("unexpected warning: ", conditionMessage(w)))

# Order of residuals ----

# TRUE if the residuals are in the order of the data, i.e. the observed response
# stored by DHARMa equals the response column of the data (see #609)
inDataOrder <- function(model, response) {
  isTRUE(all.equal(unname(getObservedResponse(model)), unname(as.numeric(response))))
}
