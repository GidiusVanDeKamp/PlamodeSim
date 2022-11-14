#' predict censoring times
#'
#' @param Data dataframe
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns KaplanMeier plot works with ggplot
#'
#' @export
#'
#'
#'
#' @importFrom rlang .data
KaplanMeierPlot <- function(Data, colour = 'grey'){#lets add an option to plot the cdf of a model with it.
  times <- Data$survivalTime%>%
    unique()%>%
    sort()
  n <- length(Data$survivalTime)
  kaplanmeier <- c(1)
  for(i in 1:(length(times)-1)){
    t <- times[i]
    DatabeforeT <- Data %>%
      dplyr::filter(.data$survivalTime <= t)
   # return(DatabeforeT)
    newInEstimator <- 1-
      (sum((dplyr::filter(DatabeforeT, .data$survivalTime == t))$outcomeCount)
       / (n-length(DatabeforeT$outcomeCount) ))
   # return(newInEstimator)
      kaplanmeier <- append(kaplanmeier, dplyr::last(kaplanmeier)* newInEstimator ) #shouldint it be only the last instead of prod

   }

  toPlot <- data.frame(times= times, kaplanmeier = kaplanmeier)
  #return(toPlot)
  #ggplot2::ggplot(toPlot, ggplot2::aes(x=times, y=kaplanmeier)) +

  return( ggplot2::geom_step(data= toPlot,mapping =ggplot2::aes(x=times, y=kaplanmeier ),colour = colour))

}
