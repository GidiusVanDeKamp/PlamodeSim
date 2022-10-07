#' simulate a new outcome
#'
#' @param data a data set as returned by newOutcomesParameters
#'
#' @return returns a boxplot for the frequencies of the outcome
#' @export
#'
greenLine <- function( plpResult,
                       covariates, 
                       studyCovariateId){
  
  # outcomeAndCovariate <- covariates$covariates %>%
  #   dplyr::group_by( hoi = (covariateId == studyCovariateId )) %>%
  #   dplyr::count() %>%
  #   dplyr::collect()
  # 
  # plpResult$prediction$outcomeCount
  
  indexOutcome <- (plpResult$prediction %>%
                           dplyr::filter( outcomeCount>= 1) %>% 
                           dplyr::select(subjectId) )[[1]]
  
  indexesCovariate <- (covariates$covariates %>%
                    dplyr::filter( covariateId == studyCovariateId) %>%
                    dplyr::select(rowId) %>%      
                    dplyr::count()%>%
                    dplyr::collect( )%>%
                    as.vector())[[1]]
  
  indexesCovariateAndOutcome <- (covariates$covariates %>%
                         dplyr::filter( covariateId == studyCovariateId & rowId %in% indexOutcome) %>%
                         dplyr::select(rowId) %>%
                          dplyr::count()%>%
                         dplyr::collect( )%>%
                         as.vector())[[1]] 
  return(indexesCovariateAndOutcome/indexesCovariate)
 }
