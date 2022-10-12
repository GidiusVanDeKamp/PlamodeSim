#' simulate a new outcome
#'
#' @param plpData a data set like the type used for plp
#' @param plpResult a data set returned by plp
#' @param parameters a data set like the type returned by plp
#'
#' @return returns a dataframe with probabilites
#' @export
#'
logNewPropsParametersPlpResult <- function( plpResult,
                                   plpData,
                                   parameters =plpResult$covariateSummary$covariateValue){
  props <-  plpResult$prediction$value
  odds <- qlogis(props)
  diffparameters<- parameters-plpResult$covariateSummary$covariateValue

  placeNewCovariates <- ((plpResult$covariateSummary$covariateValue==parameters)== FALSE) %>%
     which()

  # # REturn this PART WITH THE CORRECT PARAMETERS PLS

  # if(1 %in% placeNewCovariates){
  #   odds <- odds+ diffparameters[1]
  #   placeNewCovariates= placeNewCovariates[-1]
  # }

  for(i in placeNewCovariates ){
    covariateIds <- plpResult$covariateSummary[i,1]
    rowIdsWithCovariate <- (plpData$covariateData$covariates %>%
      dplyr::filter(covariateId == as.integer(covariateIds))%>%
      dplyr::collect() %>%
      as.vector())$rowId

    indexCovariates <- which(plpResult$prediction$rowId %in% rowIdsWithCovariate)
    # return(rowIdsWithCovariate %in% plpResult$prediction$rowId )
    # return(c(length(indexCovariates),length( rowIdsWithCovariate), unique(length( rowIdsWithCovariate))))
    for(j in 1:length(indexCovariates)){
      odds[indexCovariates[j]]<- odds[indexCovariates[j]]+ diffparameters[i]
    }
  }
  props<- plogis(odds)
  return(data.frame(newProps = props ,subjectId= plpResult$prediction$subjectId)  )
}
