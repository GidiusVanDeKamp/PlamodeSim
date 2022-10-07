#' simulate a new outcome
#'
#' @param plpResult a runPlp
#' @param noSimulations number of dataset one wants
#' @param noPersons number of persons in each data set
#' @param parameters specifies the parameters used to model new data
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
newOutcomesParameters <- function( plpResult,covariates, noSimulations, noPersons, parameters =plpResult$covariateSummary$covariateValue){
  props <-  plpResult$prediction$value
  odds <- qlogis(props)
  index <-  sample(1:length(props), noPersons*noSimulations, replace=T)
  diffparameters<- parameters-plpResult$covariateSummary$covariateValue

  placeNewCovariates <- (plpResult$covariateSummary$covariateValue==parameters)== FALSE %>%
    which()
  #loop over the covariates that are differen in the new model
  for(i in placeNewCovariates ){
    covatiateI <- plpResult$covariateSummary[i,1]
    rowIdsWithCovariateInNewSet <- (covariates$covariates %>%
      dplyr::filter(covariateId == as.integer(covatiateI)& rowId %in% index)%>%
      dplyr::select(rowId ) %>%
      dplyr::collect() %>%
      as.vector())$rowId

    for(j in 1:length(rowIdsWithCovariateInNewSet)){
      odds[rowIdsWithCovariateInNewSet[j]]<-
        odds[rowIdsWithCovariateInNewSet[j]]+ diffparameters[i]
      }
  }
  props<- plogis(odds) #dit kan efficienter
  parameters[index]

  newOutcomes <- stats::rbinom(noPersons,1, props[index])
  simulation <- c()
  for(i in 1:noSimulations){
    simulation<- append(simulation, rep(i, noPersons))
  }
  #return(simulation)
  return(data.frame(subjectId = index  ,newOutcomes= newOutcomes, simulationNumber = simulation  )  )
}
