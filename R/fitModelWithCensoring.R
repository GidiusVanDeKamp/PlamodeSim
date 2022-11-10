#' predict censoring times
#'
#' @param Trainingset the dataset used for traing the model.
#' @param modelSettings the modelSettings
#'
#' @return returns a data set with new outcomes
#' @export
#'
#' @importFrom rlang .data

fitModelWithCensoring <- function(Trainingset,  #do now Trainingset$Train
                                  modelSettings){

  fitOutcomes <- PatientLevelPrediction::fitPlp(trainData = Trainingset,
                                                modelSettings = modelSettings,
                                                analysisId = "outcome_model")

  censoringPop <- Trainingset
  censoringPop$labels <- censoringPop$labels %>%
    dplyr::mutate(
      outcomeCount = as.numeric(!(as.logical(.data$outcomeCount)))
    )

# censoringPop$labels <- censoringPop$labels %>%
#   dplyr::mutate(
#     survivalTime = timeAtRisk,
#     outcomeCount = (timeAtRisk != 7300)*1  # make this mode general this is the enddate.
#   )


  fitCensoring <- PatientLevelPrediction::fitPlp(trainData = censoringPop,
                                                 modelSettings = modelSettings,
                                                 analysisId = "censoring_model")


  return(list(censorModel = fitCensoring,
              outcomesModel = fitOutcomes))
}

