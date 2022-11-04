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
#
fitCensoring<- function(Trainingset,  #do now Trainingset$Train
                        modelSettings){

  fitOutcomes <- PatientLevelPrediction::fitPlp(trainData = Trainingset,
                                                modelSettings = modelSettings,
                                                analysisId = "outcome_model")

  censoringPop <- Trainingset
  censoringPop$labels <- censoringPop$labels %>%
    dplyr::mutate(
      outcomeCount = as.numeric(!(as.logical(outcomeCount)))
    )

  fitCensoring <- PatientLevelPrediction::fitPlp(trainData = censoringPop,
                                                 modelSettings = modelSettings,
                                                 analysisId = "censoring_model")

  return(list(censorModel = fitCensoring,
              outcomesModel = fitOutcomes))
}

