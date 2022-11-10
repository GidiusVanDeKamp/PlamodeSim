#' makes an not fitted model
#'
#'
#' @param coefficients data set with parameters with rows called betas and covariateIds
#' @param baselinehazard number of persons in the returned data set
#' @param timesofbaselinhazard data set with parameters with rows called betas and covariateIds
#' @param featureEngineering the featureEngineering
#'
#' @return returns a unfitted model
#' @export
#'
makeCoxModel<- function( coefficients,
                         baselinehazard,
                         timesofbaselinhazard,
                         featureEngineering = NULL ){

  baselineSurvival <- list(time= timesofbaselinhazard, surv= baselinehazard)
  modelType <- 'cox'
  model <- list(baselineSurvival = baselineSurvival, modelType = modelType, coefficients = coefficients)
   # $preprocessing$featureEngineering : is needed
  preprocessing <- list(featureEngineering= featureEngineering)

  Toreturn <- list(model=model,preprocessing= preprocessing)
  attr(Toreturn, 'class') <- 'plpModel'
  attr(Toreturn,"predictionFunction") <- "predictCyclops"
  attr(Toreturn,"modelType") <- "survival"
  attr(Toreturn,"saveType") <- "RtoJson"
  return(Toreturn)
}
