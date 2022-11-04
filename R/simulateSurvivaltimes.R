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
simulateSurvivaltimes <- function(plpModel, plpData, number, populationSettings){

  population <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData,
    outcomeId = 3,
    populationSettings = populationSettings
  )

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
    number
  )%>%
    return()
}
