#' box plot of frequency in the outcomes
#'
#' @param OutcomeData the data you want visualised.
#' @param noSyms number of frequencies to be calculated
#' @param noPatientsInSym number of patients to calculate the frequency with.
#' @param covariateToStudy a covariate id
#' @param plpData plpData used for finding the green dots
#' @param placeInPlot the place in the plot you want this part t
#' @param colour something that ggplot accepts as a colour.
#'
#'
#' @return returns a boxplot of the frequencies of the covariates in the dataset.
#'
#' @export
#'
#' @importFrom rlang .data
frequencyCovariatePlot <- function(OutcomeData,
                                   noSyms,
                                   noPatientsInSym,
                                   covariateToStudy,
                                   plpData,
                                   placeInPlot ,colour = 'grey'){


  rowIdsWithCov <-  (plpData$covariateData$covariates %>%
                       dplyr::filter(covariateId ==  covariateToStudy ) %>%
                       dplyr::select(rowId)%>%
                       dplyr::pull() )

  greendot <- length(rowIdsWithCov)/ nrow(plpData$cohorts)

  tempdata <- OutcomeData %>%
    dplyr::mutate(hasCovariate = 1 *(rowId %in% rowIdsWithCov))

  #force some random order
  randomIndex<- sample(noSyms*noPatientsInSym)
  tempdata <- tempdata[randomIndex, ]

  frequencies <- c()
  for(i in 0:(noSyms-1)){
    frequencies <- append(frequencies, mean(tempdata$hasCovariate[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)]))
  }

  toPlot <- data.frame(frequencies=frequencies,position= rep(placeInPlot,noSyms )  )

  return(list(
    ggplot2::geom_boxplot( data = toPlot,
                           mapping = ggplot2::aes( y=frequencies, x= position),
                           colour = colour ),
    ggplot2::geom_point( mapping = ggplot2::aes(position, frequency),
                         data= data.frame(frequency = greendot, position = placeInPlot),
                         colour =  'green')
  ))
}
