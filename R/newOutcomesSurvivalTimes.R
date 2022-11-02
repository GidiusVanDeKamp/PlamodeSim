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
newOutcomesSurvivalTimes <- function( plpModel, expbetas, number){

  index <-  sample(1:length(expbetas)[1], number, replace=T)
  uniformSample <- stats::runif(number)

  baselineSurv <- plpModel$model$baselineHazard$surv
  baselineTimes <- plpModel$model$baselineHazard$time

  props<- matrix(0,number,length(plpModel$model$baselineHazard$surv))
  for( i in 1:number){
    props[i,]<- plpModel$model$baselineHazard$surv^expbetas[index[i]]
  }

  outcomeTimes <- baselineTimes[rowSums((props>uniformSample)*1)]

  data.frame(outcome = outcomeTimes, rowId = index  ) %>%  # fails when something fails before the first time in baselinehazard.
  return()
}
