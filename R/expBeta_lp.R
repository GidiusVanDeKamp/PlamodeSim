#' hello
#'
#' @param TrainingSet a data set like the type used with plp
#' @param plpData  the plpData
#' @param populationSettings the populationSettings
#' @param plpModel a data set like the type used with plp
#'
#'
#' @return returns the expBetaz orderd by rowId
#' @export
#'
#' @importFrom rlang .data
expBeta_lp <- function(TrainingSet,
                       plpData,
                       populationSettings,
                       plpModel){

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

  prediction %>%
    dplyr::select(.data$exp_lp, .data$rowId)%>%
    return()
}
