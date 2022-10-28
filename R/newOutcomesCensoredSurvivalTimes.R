#'simulating of a survival time
#'
#' @param propMatrix matrix with the probabailies for outcomes
#' @param propMatrixCensoring matrix with the probabailies for censoring
#' @param number number of people to draw.
#' @param uniqueTimes vector with the possible outcome times.
#'
#' @return returns a data set with new outcomes
#' @export
#'
#'
newOutcomesCensoredSurvivalTimes <- function( propMatrix,propMatrixCensoring, number, uniqueTimes){
  index <-  sample(1:dim(propMatrix)[1], number, replace=T)
  uniformSample <- stats::runif(number)
  uniformSampleCensoring <- stats::runif(number)

  outcomes<-  uniqueTimes[rowSums(((propMatrix[index,]>uniformSample)*1))]

  censorTimes<- uniqueTimes[rowSums((propMatrixCensoring[index,]>uniformSampleCensoring)*1)]

  Woutcome <- (outcomes< censorTimes) %>%
              which()
  Wcensoring <- (outcomes>= censorTimes) %>%
                which()

  indexesOutcome <- index[which(outcomes< censorTimes)]
  indexesCensoring <- index[which(outcomes>= censorTimes)]

  indexes= c(indexesOutcome,indexesCensoring)
  outcomes= c(outcomes[Woutcome], censorTimes[Wcensoring])
  event= c(rep('outcome', length(indexesOutcome)),rep('censoring',length(indexesCensoring) )) #we can replase outcome with 1

  toreturn <-data.frame(rowId = indexes, event= event, outcomes=outcomes)
  return(toreturn)
}
