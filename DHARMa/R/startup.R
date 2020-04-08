
print.DHARMa.startupInfo <- function()
{ 
  version <- packageVersion('DHARMa')
  hello <- paste("This is DHARMa ",version,". For overview type '?DHARMa'." ,sep="")
  packageStartupMessage(hello)
}

.onLoad <- function(...) {
  
}

.onAttach <- function(...) { 
  print.DHARMa.startupInfo()
}

