printDHARMaStartupInfo <- function()
{
  version <- packageVersion('DHARMa')
  hello <- paste("This is DHARMa ",version,". For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa') \n Note that the simulation default has changed since version 0.4.8 for the conditional simulations of hierarchical models. If you want to switch back to the old package version defaults, please use the argument simulateREs = \"user-specified\" in simulateResiduals()." ,sep="")
  packageStartupMessage(hello)
}

.onLoad <- function(...) {
  options(DHARMaSignalColor = "red")
}

.onAttach <- function(...) {
  printDHARMaStartupInfo()
}

