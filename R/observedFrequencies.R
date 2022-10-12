#' simulate a new outcome
#'
#' @param data A data set as returned by newOutcomes
#' @param plpData a dataset
#' @param studyCovariateId the covariateId of the to study covariate
#'
#' @return returns a vector with length of number of simulations with the frequency
#'  of the outcome for the pasients with covariate of the specified covariateId
#' @export
#'

observedFrequencies <- function( data, plpData, studyCovariateId ){

  indexOutcome <- ( data %>%
                    dplyr::filter(newOutcomes == 1) %>%
                    dplyr::select(rowId))[[1]]

  withOutcomeAndCovariate <- (plpData$covariateData$covariates %>%
                             dplyr::filter( rowId %in% indexOutcome)%>%
                             collect()
                             )$rowId %>%
                             unique() %>%
                             length()

  withCovariate  <- (plpData$covariateData$covariates %>%
                    dplyr::filter( rowId %in% !!data$rowId )%>%
                    collect())$rowId %>%
                    unique() %>%
                    length()

  return(withOutcomeAndCovariate/withCovariate )
}

# observedFrequencies <- function( data, covariates, studyCovariateId ){
#   empFreq= c()
#   for(i in 1:max(data$simulationNumber)){
#
#     indexOutcomeForSim <- (data %>%
#     dplyr::filter( newOutcomes == 1, simulationNumber==i) %>%
#     dplyr::select(subjectId))[[1]]
#
#     indexForSim <- (data %>%
#     dplyr::filter( simulationNumber==i) %>%
#     dplyr::select(subjectId) %>%
#     as.vector())[[1]]
#
#     outcomeAndCovariate <- covariates$covariates %>%
#       dplyr::group_by( hasoutandcov = (rowId %in% indexOutcomeForSim ) &
#                         (covariateId == studyCovariateId )) %>%
#       dplyr::count()%>%
#       dplyr::collect()
#
#     hasCovariate <- covariates$covariates %>%
#       dplyr::group_by(  hascov = (rowId %in% indexForSim ) & (covariateId == studyCovariateId )) %>%
#       dplyr::count()%>%
#       dplyr::collect()
#
#     #return(outcomeAndCovariate)
#     if(is.na(outcomeAndCovariate[2,2]) ){
#       empFreq <- append(empFreq,0)
#     }
#     else {
#       empFreq <- (outcomeAndCovariate[2,2]/hasCovariate[2,2]) %>%
#       as.numeric() %>%
#       append(x= empFreq,)
#     }
#   }
#   return(empFreq)
#   }

