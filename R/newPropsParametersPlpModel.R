#' simulate a new outcome
#'
#' @param plpModel a data that has the type of model and the parameters and more that i dont quite understand.
#' @param plpData a data set like the type used for plp
#' @param time time for which the probabilites are calculated
#'
#'
#' @return returns a data frame with probabilities
#' @export
#'
newPropsParametersPlpModel <- function(plpModel,
                               plpData,
                               time = 10
                               ){
  if( plpModel$model$modelType == "logistic" ){
    return( PatientLevelPrediction::predictPlp(plpModel, plpData, plpData$cohorts) # one could limit the outcomes if wanted
           )
  }
  else if ( plpModel$model$modelType == "cox" ){
    return( newPropsCoxversion2(plpModel,plpData))
  }
  else{
    return(paste("models of the type",plpModel$model$modelType ,"are not yet implemented"))
  }
}
