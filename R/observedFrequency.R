#' simulate a new outcome
#'
#' @param data A data set as returned by newOutcomes
#' @param plpData a data set like the type used for plp
#' @param studyCovariateId the covariateId of the covariate for wich the frequency is wanted
#'
#' @return  the frequency of the outcome for the patients with the specified covariateId present
#' @export
#'

observedFrequency <- function( data, plpData, studyCovariateId ){

  indexOutcome <- ( data %>%
                    dplyr::filter(outcomeCount == 1) %>%
                    dplyr::select(.data$rowId))[[1]]

  withOutcomeAndCovariate <- (plpData$covariateData$covariates %>%
                             dplyr::filter( .data$rowId %in% indexOutcome, .data$covariateId == studyCovariateId )%>%
                               dplyr::collect()
                             )$rowId %>%
                             unique() %>%
                             length()


  withCovariate  <- (plpData$covariateData$covariates %>%
                    dplyr::filter( .data$rowId %in% !!data$rowId, .data$covariateId == studyCovariateId )%>%
                      dplyr::collect())$rowId %>%
                    unique() %>%
                    length() %>%
                    max(1) # sets frequencies with no covariates to zero

  return(withOutcomeAndCovariate/withCovariate )
}
