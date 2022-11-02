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
# change this fucntion so it looks more like newOutcomesSurvivalTimes.
newOutcomesCensoredSurvivalTimes <- function( plpModelCensoring, expbetasCensor, plpModel, expbetas, number){
  index <-  sample(1:length(expbetas), number, replace=T)

  uniformSample <- stats::runif(number)
  uniformSampleCensoring <- stats::runif(number)

  baselineSurv <- plpModel$model$baselineHazard$surv
  baselineSurvCensor <- plpModelCensoring$model$baselineHazard$surv

  baselineTimes       <- plpModel$model$baselineHazard$time
  baselineTimesCensor <- plpModelCensoring$model$baselineHazard$time

  props<- matrix(0,number,length(plpModel$model$baselineHazard$surv))
  propsCensoring <- matrix(0,number,length(plpModelCensoring$model$baselineHazard$surv))

  for( i in 1:number){
    props[i,]<- plpModel$model$baselineHazard$surv^expbetas[index[i]]
    propsCensoring[i,]<- plpModel$model$baselineHazard$surv^expbetasCensor[index[i]]
  }

  outcomes    <-  baselineTimes[rowSums(((props>uniformSample)*1))]
  censorTimes <-  baselineTimesCensor[rowSums((propsCensoring>uniformSampleCensoring)*1)]

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
