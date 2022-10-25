#' simulate a new outcome
#'
#' @param plpData a data set like the type used for plp
#' @param plpResult a data set returned by plp
#' @param parameters a data set like the type returned by plp
#'
#' @return returns a dataframe with probabilites
#' @export
#'
logNewPropsParametersPlpModel <- function(plpModel, #this function will be skipped
                                          plpData){
  props <-
  odds <- qlogis(props)
  diffparameters<- parameters['betas']-plpResult$model$model$coefficients['betas']

  placeNewCovariates <- ((plpResult$model$model$coefficients['betas']==parameters['betas'])== FALSE) %>%
     which()

  return(placeNewCovariates)
  for(i in placeNewCovariates ){
    covariateIds <- plpResult$covariateSummary[i,1]
    rowIdsWithCovariate <- (plpData$covariateData$covariates %>%
      dplyr::filter(covariateId == as.integer(covariateIds))%>%
      dplyr::collect() %>%
      as.vector())$rowId

    indexCovariates <- which(plpResult$prediction$rowId %in% rowIdsWithCovariate)

    for(j in 1:length(indexCovariates)){
      odds[indexCovariates[j]]<- odds[indexCovariates[j]]+ diffparameters[i]
    }
  }
  props<- plogis(odds)
  return(data.frame(newProps = props ,subjectId= plpResult$prediction$subjectId)  )
}
