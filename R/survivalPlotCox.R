#' predict censoring times
#'
#' @param plpModel the plpmodel that has an baselinesurvival
#' @param colour one can specify the colour just as in ggplot.
#' @return adds the baselinesurvival from the model to the plot
#'
#' @export
#'
#' @importFrom rlang .data

SurvivalPlotCox <- function(plpModel,colour = "red" ){

  return( ggplot2::geom_step(
    data= as.data.frame(plpModel$model$baselineSurvival),
    mapping =ggplot2::aes(x= .data$time, y= .data$surv),
    colour= colour)
  )

}
