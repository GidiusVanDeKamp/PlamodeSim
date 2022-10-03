#' Generates a new group with new outcomes
#' that follows the specified model.
#'
#' @param data Original dataset
#' @param N number of elements in your new group
#' @param parameter parameter for new generated outcomes
#'
#' @return returns a dataframe(with N rows) with ind for indexes, and newOutcomes
#' @export
#'
#'
newGroupData <- function( data, N, parameter ){
  index <-  sample(1:length(data[,1]), N, replace=T)
  ProbModel <- probsUnderModel( data , parameter, index)
  data.frame(NewOutcome =rbinom(N ,1, ProbModel),ind= index)%>%
    return()
}
