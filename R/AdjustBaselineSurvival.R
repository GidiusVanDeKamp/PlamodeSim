#' adjust an Baseline Survival function such that the overal event rate is fixed
#'
#' @param plpModel a plpModel u want to adjust
#' @param TrainingSet the training used for fitting the model, is used for adjusting the model
#' @param plpData the plpData
#' @param populationSettings the populationsettings.
#' @param timeToFixAt a time in days to fix the eventrate at. has to be in the times of the baseline survival
#' @param propToFixWith probabilities to fix
#' @param intervalSolution as this function solves an equation is needs an inteval for finding the solution.
#'
#' @return returns the same plpmodel but with a modified Baselinesurvivalfunction
#' @export
#'
#' @importFrom rlang .data

adjustBaselineSurvival <- function(plpModel,
                                   TrainingSet,
                                   plpData,
                                   populationSettings,
                                   timeToFixAt,
                                   propToFixWith,
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

  expbetaz <- prediction$exp_lp

  BaselineSurv <- plpModel$model$baselineSurvival
  BaseLinePropAtFixedTime <- BaselineSurv$surv[which(BaselineSurv$time== timeToFixAt)]

  funToSolve <- function(delta){
    mean(BaseLinePropAtFixedTime^(expbetaz*delta)) -1+ propToFixWith %>%
    return()
  }
  delta<- stats::uniroot(funToSolve, intervalSolution)$root

  newBaselineSurv <- BaselineSurv$surv^delta

  defineCoxModel( plpModel$model$coefficients,  newBaselineSurv, BaselineSurv$time )%>%
  return()
}
