#' simulate a new outcome
#'
#' @param props a vector with the probabilities ordered by rowId
#' @param noPersons number of persons in the returned data set
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
newOutcomes<- function( noPersons, props ){

  index <-  sample(1:dim(props)[1], noPersons, replace=T)
  newOutcomes <- stats::rbinom(noPersons, 1, props[index,'value'])

  return(data.frame(rowId = index, newOutcomes= newOutcomes ) ) # change the indexing
}
