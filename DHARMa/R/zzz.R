# copied from mgcv, but doesn't work

# getMessage <- function(){ 
#   library(help=DHARMa)$info[[1]] -> version
#   version <- version[pmatch("Version",version)]
#   um <- strsplit(version," ")[[1]]
#   version <- um[nchar(um)>0][2]
#   hello <- paste("This is DHARMa ",version,". For overview type 'help(\"DHARMa-package\")'.\n\nNote that, since v0.1.6.3, DHARMa includes support for glmmTMB, but there are still a few limitations with this package. In particular, due to the way glmmTMB simulates, there can be spuriuous correlations when plotting residuals against fitted. Please consult https://github.com/florianhartig/DHARMa/issues/16 for details.",sep="")
#   return(hello)
# }

.onLoad <- function(...) {

}

.onAttach <- function(...) { 
  #packageStartupMessage("Note that, since v0.1.6.2, DHARMa includes support for glmmTMB, but there are still a few minor limitations associatd with this package. Please see https://github.com/florianhartig/DHARMa/issues/16 for details, in particular if you use this for production.")
}

