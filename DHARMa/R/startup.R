print.DHARMa.startupInfo <- function()
{ 
  version <- packageVersion('DHARMa')
  hello <- paste("This is DHARMa ",version,". For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')" ,sep="")
  packageStartupMessage(hello)
}

.onLoad <- function(...) {
  
}

.onAttach <- function(...) { 
  print.DHARMa.startupInfo()
}

