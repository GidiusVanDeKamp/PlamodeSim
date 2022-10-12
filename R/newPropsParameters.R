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
