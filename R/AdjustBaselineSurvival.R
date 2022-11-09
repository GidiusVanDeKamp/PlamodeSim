#' adjust an Baseline Survival function such that the overal event rate is fixed
#'
#' @param plpModel a plpModel u want to adjust
#' @param TrainingSet the training used for fitting the model, is used for adjusting the model
#' @param plpData the plpData
#' @param populationSettings the populationsettings.
#' @param timeTofixat a time in days to fix the eventrate at. has to be in the times of the baseline survival
#' @param proptofixwith probabilities to fix
#' @param intervalSolution as this function solves an equation is needs an inteval for finding the solution.
#'
#' @return returns a modified Baselinesurvivalfunction
#' @export
#'
#' @importFrom rlang .data

AdjustBaselineSurvival <- function(plpModel,
                                   TrainingSet,
                                   plpData,
                                   populationSettings,
                                   timeTofixat,
                                   proptofixwith,
                                   intervalSolution= c(-100,100)){

  prediction <- PatientLevelPrediction::predictPlp(
    plpModel = plpModel,
    plpData = plpData,
    population = TrainingSet$labels,
    timepoint = populationSettings$riskWindowEnd
  )

  baselineSurvival <-attr(prediction, "metaData")$baselineSurvival

  prediction <- prediction %>%
    dplyr::mutate(
      exp_lp = log(1 - .data$value) / log(baselineSurvival)
    )


  expbetaz <- prediction$exp_lp # avoid this.

  BaselineSurv <- plpModel$model$baselineSurvival
  BaseLinePropAtFixedTime <- BaselineSurv$surv[which(BaselineSurv$time== timeTofixat)]

  funToSolve <- function(delta){
    mean(BaseLinePropAtFixedTime^(expbetaz*delta)) -1+ proptofixwith %>%
    return()
  }
  delta<- stats::uniroot(funToSolve, intervalSolution)$root

  newBaselineSurv <- BaselineSurv$surv^delta

  makeCoxModel( plpModel$model$coefficients,  newBaselineSurv, BaselineSurv$time )%>%
  return()
}
