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
newPropsParametersPlpResults <- function(plpResult,
                               plpData,
                               parameters =plpResult$covariateSummary$covariateValue
                               )
  {
  if( plpResult$model$trainDetails$modelName == "logistic" ){
    return(logNewPropsParametersPlpResult(plpResult,
                                          plpData,
                                          parameters )
           )
  }
  else{
    return("this type of model is not jet implemented")
  }
}
