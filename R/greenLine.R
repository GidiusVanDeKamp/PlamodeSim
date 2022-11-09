#' visual for a covariate
#'
#' @param plpData a data set like the type used in PatientLevelPredict
#' @param studyCovariateId a number, the covariateId of covariate to restrict to.
#' @param youroutcomeId the outcomeID u specified.
#'
#' @return a numer that is used for visualising.
#' @export
#' @importFrom rlang .data
#'
greenLine <- function(plpData,
                      studyCovariateId,
                      youroutcomeId){

  indexOutcome <- (plpData$outcomes %>%
                     dplyr::filter( outcomeId == youroutcomeId) %>% #change 3 to new variable called outcomeid
                     dplyr::select(.data$rowId) )[[1]]

  indexesCovariate <- (plpData$covariateData$covariates %>%
                         dplyr::filter( .data$covariateId == studyCovariateId) %>%
                         dplyr::select(.data$rowId) %>%
                         dplyr::count()%>%
                         dplyr::collect( )%>%
                         as.numeric())[[1]]

  indexesCovariateAndOutcome <- (plpData$covariateData$covariates %>%
                                   dplyr::filter( .data$covariateId == studyCovariateId & .data$rowId %in% indexOutcome) %>%
                                   dplyr::select( .data$rowId) %>%
                                   dplyr::count()%>%
                                   dplyr::collect( )%>%
                                   as.numeric())

  return(indexesCovariateAndOutcome/indexesCovariate)
}

