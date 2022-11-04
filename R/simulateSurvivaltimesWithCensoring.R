#' predict censoring times
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
simulateWithCensoring <- function(fitCensor, plpData, populationSettings,number){
  modelCensor <- fitCensor$censorModel
  modelOutcomes <- fitCensor$outcomesModel

  population <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData,
    outcomeId = 3,
    populationSettings = populationSettings
  )

  predictionOutcome <- PatientLevelPrediction::predictPlp(
    plpModel   = modelOutcomes,
    plpData    = plpData,
    population = population,
    timepoint  = populationSettings$riskWindowEnd
  )

  predictionCensor <- PatientLevelPrediction::predictPlp(
    plpModel   = modelCensor,
    plpData    = plpData,
    population = population,
    timepoint  = populationSettings$riskWindowEnd
  )

  baselineSurvivalOutcome <- attr(predictionOutcome, "metaData")$baselineSurvival
  baselineSurvivalCensor <- attr(predictionCensor, "metaData")$baselineSurvival

  predictionOutcome <- predictionOutcome %>%
    dplyr::mutate(
      exp_lp = log(1 - value) / log(baselineSurvivalOutcome) #baselineSurvival or baselineHazard
    )

  predictionCensor <- predictionCensor %>%
    dplyr::mutate(
      exp_lp = log(1 - value) / log(baselineSurvivalCensor)
    )

  newOutcomesCensoredSurvivalTimes2(
    plpModelCensoring = modelCensor,
    expbetasCensor = predictionCensor$exp_lp ,
    plpModel = modelOutcomes,
    expbetas = predictionOutcome$exp_lp,
    number
  )%>%
  return()

}


