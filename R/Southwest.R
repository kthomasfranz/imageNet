#' @title SouthWest Airline Routes, 1987-97
#' @description Network of SouthWest airlines routes between 1987 and 1997
#' @format A data frame with 236 rows and 4 variables:
#' \describe{
#'   \item{\code{CARRIER}}{character, the airline carrier (which is always WN, i.e., SouthWest)}
#'   \item{\code{YEAR}}{character, lists whether the link between nodes/airports existed only in 1987,
#'   only in 1997, or both periods (“ALWAYS”)}
#'   \item{\code{ORIGIN}}{character, the 3-letter codes of the origin airport}
#'   \item{\code{DEST}}{character, the 3-letter codes of the destination airport}
#'}
#' @details Edges data frame for Southwest network data.
#' @source US Bureau of Transportation Statistics: Airline On-Time Performance Database
#' @docType data
#' @keywords datasets
#' @name Southwest
#' @usage Southwest
NULL
