#' simulate a new outcome
#'
#' @param plpResult a runPlp
#' @param covariates should be removed
#' @param parameters specifies the parameters used to model new data
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
logNewPropsParameters <- function( plpData,
                                   parameters ){

  odds<- rep(parameters[1,1], max(plpData$cohorts$rowId)) #can this be improved?

  for(i in 2:dim(parameters)[1] ){
    if (parameters[i,1]!=0 ){
      covariateIds <- param[i,2]
      rowIdsWithCovariate <- (plpData$covariateData$covariates %>%
                                dplyr::filter(covariateId == as.integer(covariateIds))%>%
                                dplyr::collect() %>%
                                as.vector())$rowId
      odds[rowIdsWithCovariate]<-  odds[rowIdsWithCovariate]+ parameters[i,1]
    }
  }
  props<- plogis(odds)
  return(data.frame(newProps = props )  )
}

