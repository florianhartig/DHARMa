#' Hurricanes
#'
#' A dataset on hurricane strength and fatalities in the US between 1950 and 2012. The data originates from the study by Jung et al., PNAS, 2014, who claim that the masculinity / femininity of a hurricane name has a causal effect on fatalities, presumably through a different perception of danger caused by the names.
#'
#' @name hurricanes
#' @aliases hurricanes
#' @docType data
#'
#' @references Jung, K., Shavitt, S., Viswanathan, M., & Hilbe, J. M. (2014). Female hurricanes are deadlier than male hurricanes. Proceedings of the National Academy of Sciences, 111(24), 8782-8787.
#'
#' @format A 'data.frame': 92 obs. of  14 variables
#' \describe{
#'   \item{Year}{Year of the hurricane (1950-2012) }
#'   \item{Name}{Name of the hurricane }
#'   \item{MasFem}{Masculinity-femininity rating of the hurricane's name in the range 1 = very masculine, 11 = very feminine.}
#'   \item{MinPressure_before}{Minimum air pressure (909-1002).}
#'   \item{Minpressure_Updated_2014}{Updated minimum air pressure (909-1003).}
#'   \item{Gender_MF}{Binary gender categorization based on MasFem (male = 0, female = 1).}
#'   \item{Category}{Strength of the hurricane in categories (1:7). (1 = not at all, 7 = very intense).}
#'   \item{alldeaths}{Human deaths occured (1:256).}
#'   \item{NDAM}{Normalized damage in millions (1:75.000). The raw (dollar) amounts of property damage caused by hurricanes were obtained, and the unadjusted dollar amounts were normalized to 2013 monetary values by adjusting them to inflation, wealth and population density.}
#'   \item{Elapsed_Yrs}{Elapsed years since the occurrence of hurricanes (1:63).}
#'   \item{Source}{MWR/wikipedia ()}
#'   \item{ZMasFem}{Scaled (MasFem)}
#'   \item{ZMinPressure_A}{Scaled (Minpressure_Updated_2014)}
#'   \item{ZNDAM}{Scaled (NDAM)}
#'   ...
#' }
#' @example inst/examples/hurricanes.R
NULL
