#' hurricanes
#'
#' This dataset contains hurricanes in the USA between 1950 and 2012 their strength, names ..., see parameter for more information
#' @name hurricanes
#' @title hurricanes dataset residual analysis
#' @aliases {hurricanes}
#' @docType data
#' @keywords hurricanes, DHARMa, simulateResiduals,
#'
#'
#' @param Year. Year the hurricanes appeared (1950-2012)
#' @param Name. Name the hurricanes was named after (name)
#' @param MasFem. Rating of the Name, if it is more masculin or feminine (1:11). In the range 1 = very masculine, 11 = very feminine
#' @param MinPressure_before. Minimum air-pressure (909-1002)
#' @param `Minpressure_Updated 2014`. Minimum airpressure (909-1003)
#' @param Gender_MF. Gender rating binary (male/female). Male = 0, female = 1)
#' @param Category. strength of the hurricane in categories (1:7). (1 = not at all, 7 = very intense)
#' @param alldeaths. Deaths counted (1:256)
#' @param NDAM. normalized damage in millions (1:75.000). The raw (dollar) amounts of property damage caused by hurricanes were obtained, and the unadjusted dollar amounts were normalized to 2013 monetary values by adjusting them to inflation, wealth and population density
#' @param `Elapsed Yrs`. elapsed since the occurrence of hurricanes (1:63)
#' @param Source. MWR/wikipedia ()
#' @param ZMasFem. scaled (MasFem)
#' @param ZMinPressure_A. scaled (Minpressure_Updated 2014)
#' @param ZNDAM. scaled (NDAM)
#'
#' @return a dataset
#' @usage{data(hurricanes)} Jung et al., PNAS, 2014
#' @references \link{Jung et al., PNAS, 2014}
#' @seealso \code{\link{https://theoreticalecology.wordpress.com/2021/04/17/hurricanes-and-himmicanes-revisited-with-dharma/}}
#' @format A data frame with 93 rows and 14 variables
#' @example inst/examples/hurricanes.R
NULL
