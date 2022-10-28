#'simulating of a survival time
#'
#' @param propMatrix matrix with the probabailies
#' @param number number of people to draw.
#' @param uniqueTimes vector with the possible outcome times.
#'
#' @return returns a data set with new outcomes
#' @export
#'
#'
newOutcomesSurvivalTimes <- function( propMatrix, number, uniqueTimes){
  index <-  sample(1:dim(propMatrix)[1], number, replace=T)
  uniformSample <- stats::runif(number)

  outcome <- uniqueTimes[rowSums((propMatrix[index,]>uniformSample)*1)]

  data.frame(outcome = outcome, rowId = index  ) %>%
  return()
}
