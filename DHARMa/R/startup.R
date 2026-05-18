printDHARMaStartupInfo <- function()
{
  version <- packageVersion('DHARMa')
  hello <- paste("This is DHARMa ",version,". For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa') \n\nNote that the default setting in simulateResiduals was changed to conditional simulations since version 0.5.0. This is likely to change residual calculations for all hierarchical (in particular random effect) models. If you want to switch back to the old package version defaults, please use the argument simulateREs = \"user-specified\" in simulateResiduals(). For more details, see ?simulateREsiduals." ,sep="")
  packageStartupMessage(hello)
}

.onLoad <- function(...) {
  options(DHARMaSignalColor = "red")
}

.onAttach <- function(...) {
  printDHARMaStartupInfo()
}

