#'simulating of a survival time
#'
#' @param plpModel model with
#' @param number number of people to draw.
#' @param expbetas expbetas.
#'
#' @return returns a data set with new outcomes
#' @export
#'
#'
newOutcomesSurvivalTimes2 <- function( plpModel, expbetas, number){

  index <-  sample(1:length(expbetas)[1], number, replace=T)
  uniformSample <- stats::runif(number)

  baselineSurv <- plpModel$model$baselineSurvival$surv
  baselineTimes <- plpModel$model$baselineSurvival$time

  props<- matrix(0,number,length(plpModel$model$baselineSurvival$surv))
  for( i in 1:number){
    props[i,]<- plpModel$model$baselineSurvival$surv^expbetas[index[i]]
  }

  baselineTimes<- c(baselineTimes,Inf)

  outcomeTimes <- baselineTimes[rowSums((props>uniformSample)*1)+1]

  data.frame(outcome = outcomeTimes, rowId = index  ) %>%  # fails when something fails before the first time in baselinehazard.
  return()
}
