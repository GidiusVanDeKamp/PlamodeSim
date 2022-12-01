#' predict censoring times
#'
#' @param Data dataframe
#' @param covariateToStudy a covaraite id
#' @param plpData the covariate Data
#' @param colour something that ggplot accepts as a colour.
#'
#'
#' @return returns KaplanMeier plot works with ggplot the data has been filtered
#'  such that only the covariateToStudy is present
#'
#' @export
#'
#'
#' @importFrom rlang .data
#does not work now!
KaplanMeierPlotFilterCovariate <- function( Data, covariateToStudy, plpData, colour = 'grey'){

  rowIdsWithCov <-  (plpData$covariateData$covariates %>%
                       dplyr::filter(covariateId ==  covariateToStudy ) %>%
                       dplyr::select(rowId)%>%
                       dplyr::pull() )

  Data <- Data %>%
    dplyr::filter(rowId %in% rowIdsWithCov)


  times <- Data$survivalTime%>%
    unique()%>%
    sort()

  n <- length(Data$survivalTime)
  kaplanmeier <- c(1)

  for(i in 1:(length(times)-1)){
    t <- times[i]
    DatabeforeT <- Data %>%
      dplyr::filter(.data$survivalTime <= t)
    newInEstimator <- 1-
      (sum((dplyr::filter(DatabeforeT, .data$survivalTime == t))$outcomeCount)
       / (n-length(DatabeforeT$outcomeCount) ))
      kaplanmeier <- append(kaplanmeier, dplyr::last(kaplanmeier)* newInEstimator )

   }
  toPlot <- data.frame(times = times, kaplanmeier = kaplanmeier)

  return( ggplot2::geom_step(
    data = toPlot,
    mapping = ggplot2::aes(x=times, y=kaplanmeier),
    colour = colour
  ))

}
