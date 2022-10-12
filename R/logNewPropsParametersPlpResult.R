#' simulate a new outcome
#'
#' @param plpResult a runPlp
#' @param covariates should be removed
#' @param parameters specifies the parameters used to model new data
#'
#' @return returns a dataframe with newOutcomes and subjectId
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

  # # the first parameter is not the intercept is it? i dont see an intercept,
  # # maybe it is a bit redundant any way sinds sex is listed twice in the covariates.
  # # plpResult$covariateSummary$covariateName[53:54]

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
