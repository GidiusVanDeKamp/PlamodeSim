#' simuates a new dataset.
#'
#' @param outcomeId the outcomeID
#' @param plpData the plpdata
#' @param outcomeId the outcome id
#' @param populationSettings as made with createStudyPopulationSettings()
#' @param splitSettings as made with createDefaultSplitSetting()
#' @param sampleSettings  as made with createSampleSettings ()
#' @param featureEngineeringSettings as made with createFeatureEngineeringSettings()
#' @param preprocessSettings as made with createPreprocessSettings()
#' @param modelSettings specifies your chosen model like setCoxModel()
#' @param executeSettings as made with createExecuteSettings()
#' @param numberToSimulate number you choose # should in the future be sample settings
#'
#' @return returns a new dataset
#' @export
#'
#'
runPlasmode <- function(plpData,
                        outcomeId,
                        populationSettings,
                        splitSettings,
                        sampleSettings,
                        featureEngineeringSettings,
                        preprocessSettings,
                        modelSettings,
                        executeSettings,
                        numberToSimulate){

  TrainingSet <- PlasmodeSim::MakeTraingSet(
    plpData = plpData,
    executeSettings = executeSettings,
    populationSettings = populationSettings,
    splitSettings = splitSettings,
    sampleSettings = sampleSettings,
    preprocessSettings = preprocessSettings,
    featureEngineeringSettings = featureEngineeringSettings,
    outcomeId = outcomeId
  )

  fitCensor <- PlasmodeSim::fitModelWithCensoring(
    Trainingset = TrainingSet$Train,
    modelSettings = modelSettings
    # now i have only one model setting
    # should i change this to two seperate settings
  )

  NewOutcomes <- PlasmodeSim::simulateSurvivaltimesWithCensoring(
    censorModel = fitCensor,
    plpData = plpData,
    population = TrainingSet$Train$labels,
    populationSettings = populationSettings,
    numberToSimulate = numberToSimulate
  )
  return(NewOutcomes)
}

