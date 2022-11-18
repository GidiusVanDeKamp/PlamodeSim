#' predict censoring times
#'
#' @param Data dataframe
#' @param covariateToStudy a covariate id the patients are restricted the patients that have this covariate
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns KaplanMeier plot works with ggplot
#'
#' @export
#'
#'
#'
#' @importFrom rlang .data
KaplanMeierPlotCovariate <- function(Data ,
                            covariateToStudy,
                            covariateData,
                            colour = 'grey'){#lets add an option to plot the cdf of a model with it.

  restrictRowId<- (covariateData$covariates %>%
                     filter( rowId %in% !! Data$rowId  &  covariateId ==  covariateToStudy ))%>%
    select(rowId)%>%
    collect()

# this part doesnt work jet
  Data <- Data %>%
    filter( rowId %in% as.vector(restrictRowId))%>%
    collect()

  times <- Data$survivalTime%>%
    unique()%>%
    sort()
  n <- length(Data$survivalTime)
  kaplanmeier <- c(1)
  #return(Data)
  for(i in (1:length(times))){ # here is the problem
    t <- times[i]
    #return(Data)

    DatabeforeT <- Data %>% dplyr::filter( .data$survivalTime <= t) #.data$
    newInEstimator <- 1-
      (sum((dplyr::filter(DatabeforeT, .data$survivalTime == t))$outcomeCount)
       / (n-length(DatabeforeT$outcomeCount) ))

      kaplanmeier <- append(kaplanmeier, dplyr::last(kaplanmeier)* newInEstimator )
   }

  toPlot <- data.frame(times= times, kaplanmeier = kaplanmeier[-1])
  #return(toPlot)
  #ggplot2::ggplot(toPlot, ggplot2::aes(x=times, y=kaplanmeier)) +

  return( ggplot2::geom_step(data= toPlot,mapping =ggplot2::aes(x=times, y=kaplanmeier ),colour = colour))

}

#utils global vsariables . use this method to do remove the warings of the devtools::check().
