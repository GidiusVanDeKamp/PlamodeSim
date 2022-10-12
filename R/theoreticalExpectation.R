#' frequency of a covariate in a dataset
#'
#' @param plpData a data set
#' @param covariateId number of a covariate id
#'
#' @return returns a number between 0 and 1
#' @export
#'
#'
theoreticalExpectation<- function(plpData,
                                  parameters
                                  ){
  redlines <- parameters[1,1]
  paramNonZero <- which(parameters[1]!=0)
  for(i in 2:length(paramNonZero)){
    redlines <- redlines+ frequencyCovariatePlpData(plpData,parameters[paramNonZero[i],2])*parameters[paramNonZero[i],1]
  }
  redlines <- plogis(redlines)
  return(redlines)
  }

frequencyCovariatePlpData<- function(plpData,  #doesnt work correctly
                                     covariateIdToStudy
                                     ){
  noOfcov <-  plpData$covariateData$covariates %>%
    filter(covariateId == covariateIdToStudy) %>%
    count() %>%
    collect() %>%
    as.integer()
  # return(
  #   plpData$covariateData$covariates %>%
  #     filter(covariateId == covariateIdToStudy) %>%
  #     collect(rowId) %>%
  #     unique() %>%
  #     count()
  # )
  totalNo <- max(plpData$cohorts$rowId)
  return(noOfcov/(totalNo))
}
