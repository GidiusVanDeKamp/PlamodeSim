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
# change this function so it looks more like newOutcomesSurvivalTimes.
newOutcomesCensoredSurvivalTimes2 <- function( plpModelCensoring, expbetasCensor, plpModel, expbetas, number){
  index <-  sample(1:length(expbetas), number, replace=T)

  uniformSample <- stats::runif(number)
  uniformSampleCensoring <- stats::runif(number)

  baselineSurv <- plpModel$model$baselineSurvival$surv
  baselineSurvCensor <- plpModelCensoring$model$baselineSurvival$surv

  baselineTimes       <- plpModel$model$baselineSurvival$time
  baselineTimesCensor <- plpModelCensoring$model$baselineSurvival$time

  props<- matrix(0,number,length(plpModel$model$baselineSurvival$surv))
  propsCensoring <- matrix(0,number,length(plpModelCensoring$model$baselineSurvival$surv))

  for( i in 1:number){
    props[i,]<- plpModel$model$baselineSurvival$surv^(expbetas[index[i]])
    propsCensoring[i,]<- plpModelCensoring$model$baselineSurvival$surv^(expbetasCensor[index[i]])
  }

  baselineTimes <-c(0,baselineTimes)
  baselineTimesCensor <-c(0,baselineTimesCensor)

  outcomes    <-  baselineTimes[rowSums(((props>uniformSample)*1))+1]
  censorTimes <-  baselineTimesCensor[rowSums((propsCensoring>uniformSampleCensoring)*1)+1]

  #return(list(censorTimes, outcomes))

  Woutcome <- (outcomes< censorTimes) %>%
              which()
  Wcensoring <- (outcomes>= censorTimes) %>%
                which()

  indexesOutcome <- index[which(outcomes< censorTimes)]
  indexesCensoring <- index[which(outcomes>= censorTimes)]

  indexes= c(indexesOutcome,indexesCensoring)
  outcomes= c(outcomes[Woutcome], censorTimes[Wcensoring])
  outcomeCount= c(rep(1, length(indexesOutcome)),rep(0,length(indexesCensoring) )) #we can replase outcome with 1

  toreturn <-data.frame(rowId = indexes, outcomeCount= outcomeCount, outcomes=outcomes)
  return(toreturn)
}

