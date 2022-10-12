#' simulate a new outcome
#'
#' @param plpData  a data set as used for plp
#' @param parameters parameters as returned by plp
#' @param modelName now it only works for "logistic"
#'
#' @return returns a dataframe with probabilties d
#' @export
#'
newPropsParameters <- function(plpData,
                               parameters,
                               modelName
                               )
  {
  if( modelName == "logistic" ){
    return(logNewPropsParameters(plpData,
                                 parameters )
           )
  }
  else{
    return("this type of model is not jet implemented")
  }
}
