#' adjust an Baseline Survival function such that the overal event rate is fixed
#'
#' @param BaselineSurv a data frame with the the colums called surv and time.
#' @param timeTofixat a time in days to fix the eventrate at. has to be in the times of the baseline survival
#' @param expbetaz the vector with expbetasz made with function expBetaZ()
#' @param intervalSolution the inteval used for finding the solution.
#' @param proptofixwith probabilitie of the event something somethign
#'
#' @return returns a modified Baselinesurvivalfunction
#' @export
#'
AdjustBaselineSurvival <- function(BaselineSurv,
                                   timeTofixat,
                                   proptofixwith,
                                   expbetaz,
                                   intervalSolution= c(-100,100)){
  BaseLinePropAtFixedTime <- BaselineSurv$surv[which(BaselineSurv$time== timeTofixat)]

  funToSolve <- function(delta){
    mean(BaseLinePropAtFixedTime^(expbetaz*delta)) -1+ proptofixwith %>%
    return()
  }
  #return(funToSolve(intervalSolution[1]))
  delta<- uniroot(funToSolve, intervalSolution)$root

  newBaselineSurv <- BaselineSurv$surv^delta

  return(list(surv= newBaselineSurv, time= BaselineSurv$time))

}
