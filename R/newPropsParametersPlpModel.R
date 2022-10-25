#' simulate a new outcome
#'
#' @param plpData a data set like the type used for plp
#' @param plpResult a data set returned by plp
#' @param parameters a data set like the type used for plp
#'
#'
#' @return returns a dataframe with probabilites
#' @export
#'
newPropsParametersPlpModel <- function(plpModel,
                               plpData
                               ){

  if( plpModel$model$modelType == "logistic" ){
    return( predictPlp(plpModel, plpData, plpData$cohorts) # one could limit the outcomes if wanted
           )
  }
  else if ( plpModel$model$modelType == "cox" ){
    return( "coming soon")
  }
  else{
    return(paste("models of the type",plpModel$model$modelType ,"are not yet implemented"))
  }
}
