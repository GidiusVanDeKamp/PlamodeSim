#' predict censoring times
#'
#' @param plpModel
#' @return returns survivalfunction ro plot
#'
#' @export
#'
#'
#'
#' @importFrom rlang .data
SurvivalPlotCox <- function(plpModel,colour = "red" ){

  return( ggplot2::geom_step(
    data= as.data.frame(plpModel$model$baselineSurvival),
    mapping =ggplot2::aes(x=time, y=surv),
    colour= colour)
  )

}
