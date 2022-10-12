#' simulate a new outcome
#'
#' @param plpResult a runPlp
#' @param noSimulations number of dataset one wants
#' @param noPersons number of persons in each data set
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
newOutcomes<- function( noPersons, props ){

  index <-  sample(1:length(props), noPersons, replace=T)
  newOutcomes <- stats::rbinom(noPersons, 1, props[index])

  return(data.frame(rowId = index, newOutcomes= newOutcomes ) )
}
# newOutcomes( 15, newPropsParameters(plpData ,covariates, param)$newProps)
