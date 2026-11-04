# Adding support for new R packages to DHARMa

I receive a lot of [requests for supporting further R packages with DHARMa](https://github.com/florianhartig/DHARMa/labels/packageSupport), so here are a few general technical comments regarding this topic:

First of all, if you have an unsupported model, there are two options to create DHARMa residual plots

1. Making your package compatible with DHARMa
2. Simulate new data from a fitted model by hand, and use the createDHARMa function to read those simulations into DHARMa 


If feasible, option 1 is obviously preferable, so I'll start with this, followed by the fallback option 2.

## Integrating a package with DHARMa

DHARMa interacts with other regression packages through a number of S3 wrapper functions which are available and described in the file [compatibility.R](https://github.com/florianhartig/DHARMa/blob/master/DHARMa/R/compatibility.R). 

For each wrapper, there is a default function. These default functions essentially assume that the following functions / options are implemented and work as for glm / lme4. 

* predict(object) - including type = response for GLMs, and ideally using the re.form syntax of lme4 to condition on REs
* simulate(object) - ideally using the re.form syntax of lme4 to condition on REs
* coef(object)
* residuals(object) - implementing raw and Pearson residuals
* logLik(object)
* object$family available, with the family object being implemented in the standard structure
* model.frame(object), with response being in the 1st column


If your model class implements these points identically to lm/glm/lme4, chances are good that DHARMa will work out of the box, even if a warning is produced. 

If that is not the case, you (or I) have to create a new S3 wrapper (e.g. getSimulations.YourClass) to extract simulations from the fitted model using your model syntax. Of course, this assumes that the respective functions / options are implemented by your model class in the first place. Regarding the latter, my strong preference is that:

* I maintain the DHARMa wrappers, and DHARMa wrappers NEVER perform model-specific calculations, they only deal with the interface (i.e. variable / function names)
* If a certain functionality is not included in the original model class, or if the functionality is faulty, I will not fix it on the DHARMa side by writing new code, this needs to be fixed by the developers of the respective package.

The sense of this separation of work is that functions related to a specific package should stay and be maintained with this package, not with DHARMa. 

Usually I write the wrapper function myself, it's enough that you provide the S3 functions needed for DHARMa with your model, but of course you can also create a (tested) pull request with all the necessary wrapper functions, which would allow me to integrate your package seamlessly into DHARMa. 

## Simulating new data by hand and reading this into DHARMa

If your model is not supported by DHARMa, you can still simulate data by hand and read this into DHARMa, allowing you to perform most of the residual checks implemented. This is easiest if your model already has a simulate function implemented, which allows creating new data based on the assumptions and parameters of a fitted statistical model, as in:

```r
fit <- glm(...)
newData <- simulate(fit)
```
A simulate function is a very useful extension of any advanced regression package, as it allows performing a number of essential tasks for technical validation, statistical validation, and non-parametric methods, such as

* Technical validation / unit tests
* Power analysis
* Analysis of bias or coverage
* Calculations of CIs via parametric bootstrap
* Simulated residuals implemented in DHARMa

If a simulate function is not available, the best option would be to **ask the maintainers** of the respective regression package to create a simulate function, or create one yourself and make a pull request. 

Note for developers: 

* I recommend that the signature and output format should conform to the existing implementations in glm and lme4, which is simulate(fittedModel, nsim, ...)
* ... should ideally allow changing the conditioning of the data creation on all hierarchical levels of the model, which includes being able to condition on the REs, as well as possibly on things like zero-inflation etc.

If a simulate function is not available, you could still take out the parameters of the fitted model, and simulate new data according to these parameters. Here is an example for a Poisson GLM:

```r
testData = createData(sampleSize = 50, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = "poisson")

# in DHARMa, using the simulate.glm function of glm 
sims = simulateResiduals(fittedModel)
plot(sims, quantreg = FALSE)

# Doing the same with a hand-coded simulate function. 
# of course this code will only work with a 1-par glm model
simulateMyfit <- function(n=10, fittedModel){
  int = coef(fittedModel)[1]
  slo = coef(fittedModel)[2]
  pred = exp(int + slo * testData$Environment1)
  predSim = replicate(n, rpois(length(pred), pred))
  return(predSim)
}

sims = simulateMyfit(250, fittedModel)

dharmaRes <- createDHARMa(simulatedResponse = sims, observedResponse = testData$observedResponse, fittedPredictedResponse = predict(fittedModel, type = "response"), integer = T)
plot(dharmaRes)
```
Another example [here](https://aosmith.rbind.io/2017/12/21/using-dharma-for-residual-checks-of-unsupported-models) 

Once you have an option to simulate, you can always read in the simulations via createDHARMa, which allows using most options of the DHARMa package.
