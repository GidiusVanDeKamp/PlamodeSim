#' makes an not fitted model
#'
#'
#' @param parameters data set with parameters with rows called betas and covariateIds
#' @param modelname number of persons in the returned data set
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
makeCoxModel<- function( coefficients, baselinehazard, timesofbaselinhazard ){
  baselineSurvival <- list(time= timesofbaselinhazard, surv= baselinehazard)
  modelType <- 'cox'
  model <- list(baselineSurvival = baselineSurvival, modelType = modelType, coefficients = coefficients)
   # $preprocessing$featureEngineering : is needed
  preprocessing <- list(featureEngineering= NULL)
  return(list(model=model,preprocessing= preprocessing))
}
