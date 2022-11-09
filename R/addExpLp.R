#' adds the expLp to a prediction
#'
#' @param prediction form a predictPlp
#' @param baselineSurvival the baselinesurvival
#'
#'
#' @return returns the prediction with an extra column
#' @export
#'
#' @importFrom rlang .data
addExpLp <- function(prediction,baselineSurvival){
  prediction %>%
    dplyr::mutate(
      exp_lp = log(1 - .data$value) / log(baselineSurvival)
    )%>%
  return()
}
