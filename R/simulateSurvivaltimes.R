#'simulating of a survival time
#'
#' @param plpModel model with
#' @param numberToSimulate number of people to draw.
#' @param expbetas expbetas.
#'
#' @return returns a data set with new outcomes
#' @export
#'
#'
simulateSurvivaltimes <- function(plpModel,
                                  plpData,
                                  numberToSimulate,
                                  population,
                                  populationSettings){ # population.

  predictionOutcome <- PatientLevelPrediction::predictPlp(
    plpModel   = plpModel,
    plpData    = plpData,
    population = population,
    timepoint  = populationSettings$riskWindowEnd
  )

  baselineSurvivalOutcome <- attr(predictionOutcome, "metaData")$baselineSurvival

  predictionOutcome <- predictionOutcome %>%
    dplyr::mutate(
      exp_lp = log(1 - value) / log(baselineSurvivalOutcome) #baselineSurvival or baselineHazard
    )

  newOutcomesSurvivalTimes2(
    plpModel,
    predictionOutcome$exp_lp,
    numberToSimulate
  )%>%
    return()
}
