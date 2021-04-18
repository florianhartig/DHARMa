#' hurricanes
#'
#' This dataset contains hurricanes in the USA between 1950 and 2012 their strength, names ..., see parameter for more information
#' @name hurricanes
#' @docType data
#' @keywords hurricanes, DHARMa, simulateResiduals,
#'
#' @param Year. Year the hurricanes appeared (1950-2012)
#' @param Name. Name the hurricanes was named after (name)
#' @param MasFem. Rating of the Name, if it is more masculin or feminine (1:11). In the range 1 = very masculine, 11 = very feminine
#' @param MinPressure_before. Minimum air-pressure (909-1002)
#' @param Minpressure_Updated.2014. Minimum airpressure (909-1003)
#' @param Gender_MF. Gender rating binary (male/female). Male = 0, female = 1)
#' @param Category. strength of the hurricane in categories (1:7). (1 = not at all, 7 = very intense)
#' @param alldeaths. Deaths counted (1:256)
#' @param NDAM. normalized damage in millions (1:75.000). The raw (dollar) amounts of property damage caused by hurricanes were obtained, and the unadjusted dollar amounts were normalized to 2013 monetary values by adjusting them to inflation, wealth and population density
#' @param Elapsed Years elapsed since the occurrence of hurricanes (1:63)
#' @param Source. MWR/wikipedia ()
#' @param ZMasFem. scaled (MasFem)
#' @param ZMinPressure_A. scaled (Minpressure_Updated 2014)
#' @param ZNDAM. scaled (NDAM)
#'
#' @return a dataset
#' @usage [data(hurricanes)]
#' @references \link{https://theoreticalecology.wordpress.com/2021/04/17/hurricanes-and-himmicanes-revisited-with-dharma/}
#' @seealso Jung et al., PNAS, 2014
#' @format A data frame with 93 rows and 14 variables
#'
#' @examples
#'
#' hurricanes <- readxl::read_excel('data-raw/Hurricane.xlsx',
#'               range = "A1:N93",
#'               col_names = TRUE,
#'               na = "(NA)")
#'
#'
#' Data <- hurricanes
#'
#' library(glmmTMB)
#'
#'Data <- hurricanes
#' originalModelGAM = glmmTMB(alldeaths ~ scale(MasFem) *
#'                             (scale(Minpressure_Updated.2014) + scale(NDAM)),
#'                           data = Data, family = nbinom2)
#'
#'
#' # Residual checks with DHARMa
#' library(DHARMa)
#' res <- simulateResiduals(originalModelGAM)
#' plot(res)
#'
#' # no significant deviation in the general plot, but try this
#' # which was highlighted by https://www.theguardian.com/science/grrlscientist/2014/jun/04/hurricane-gender-name-bias-sexism-statistics
#' plotResiduals(res, Data$NDAM)
#'
#' # we also find temporal autocorrelation
#' res2 = recalculateResiduals(res, group = Data$Year)
#' testTemporalAutocorrelation(res2, time = unique(Data$Year))
#'

NULL
