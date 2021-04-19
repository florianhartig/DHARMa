#' hurricanes
#'
#' This dataset contains hurricanes in the USA between 1950 and 2012 their strength, names ..., see parameter for more informations.
#'
#' @name hurricanes
#' @title hurricanes dataset residual analysis
#' @aliases hurricanes
#' @docType data
#' @keywords hurricanes, DHARMa, simulateResiduals,
#'
#' @source https://theoreticalecology.wordpress.com/2021/04/17/hurricanes-and-himmicanes-revisited-with-dharma/
#' @references Jung et al., PNAS, 2014
#' @seealso \code{\link{simulateResiduals}}
#'
#' @format A 'data.frame': 93 obs. of  14 variables
#' \describe{
#'   \item{Year}{Year the hurricanes appeared (1950-2012)}
#'   \item{Name}{Name the hurricanes was named after (name)}
#'   \item{MasFem}{Rating of the Name, if it is more masculin or feminine (1:11). In the range 1 = very masculine, 11 = very feminine}
#'   \item{MinPressure_before}{Minimum air-pressure (909-1002)}
#'   \item{Minpressure_Updated_2014}{Minimum air-pressure (909-1003)}
#'   \item{Gender_MF}{eGender rating binary (male/female). Male = 0, female = 1)}
#'   \item{Category}{strength of the hurricane in categories (1:7). (1 = not at all, 7 = very intense)}
#'   \item{alldeaths}{Deaths counted (1:256)}
#'   \item{NDAM}{normalized damage in millions (1:75.000). The raw (dollar) amounts of property damage caused by hurricanes were obtained, and the unadjusted dollar amounts were normalized to 2013 monetary values by adjusting them to inflation, wealth and population density}
#'   \item{Elapsed_Yrs}{elapsed since the occurrence of hurricanes (1:63)}
#'   \item{Source}{MWR/wikipedia ()}
#'   \item{ZMasFem}{scaled (MasFem)}
#'   \item{ZMinPressure_A}{scaled (Minpressure_Updated_2014)}
#'   \item{ZNDAM}{scaled (NDAM)}
#'   ...
#' }
#' @example inst/examples/hurricanes.R
NULL
