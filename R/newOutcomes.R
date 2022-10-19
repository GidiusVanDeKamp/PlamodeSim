#' simulate a new outcome
#'
#' @param props a vector with the probabilities order by rowId
#' @param noPersons number of persons in the returned data set
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
newOutcomes<- function( noPersons, props ){

  index <-  sample(1:dim(props)[1], noPersons, replace=T)
  newOutcomes <- stats::rbinom(noPersons, 1, props[index,1])

  return(data.frame(rowId = index, newOutcomes= newOutcomes ) )
}
# newOutcomes( 15, newPropsParameters(plpData ,covariates, param)$newProps)
