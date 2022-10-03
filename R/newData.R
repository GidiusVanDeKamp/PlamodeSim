#new outcome for specified group
#' Title
#'
#' @param data orginal dataset
#' @param parameter parameters used to obtain new data
#' @param Indexes a vector of indexes of the patients u want new outcomes for
#'
#' @return returns a dataframe with newOutcomes and indexes
#' @export
#'
newData <- function( data, parameter, indexes ){
  probModel <- probsUnderModel( data , parameter, indexes )
  data.frame(newOutcome =rbinom(length(indexes), 1, probModel),ind= indexes) %>%
    return()
}
