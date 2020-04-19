print.DHARMa.startupInfo <- function()
{ 
  version <- packageVersion('DHARMa')
  hello <- paste("This is DHARMa ",version,". For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')", "Note: Syntax of plotResiduals has changed in 0.3.0, see ?plotResiduals for details" ,sep="")
  packageStartupMessage(hello)
}

.onLoad <- function(...) {
  
}

.onAttach <- function(...) { 
  print.DHARMa.startupInfo()
}

