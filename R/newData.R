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
newData <- function( data, parameter, Indexes ){
  ProbModel <- ProbsUnderModel( data , parameter, Indexes )
  data.frame(newOutcome =rbinom(length(Indexes), 1, ProbModel),ind= indexes) %>%
    return()
}
