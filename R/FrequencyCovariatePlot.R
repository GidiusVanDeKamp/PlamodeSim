#' box plot of frequency in the outcomes
#'
#' @param Data dataframe
#' @param covariateToStudy a covaraite id
#' @param plpData the covariate Data
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns a boxplot of the frequencies of the covariates in the dataset.
#'
#' @export
#'
#' @importFrom rlang .data
frequencyCovariatePlot <- function(tempdata,
                                    noSyms,noPatientsInSym, covariateToStudy, plpData, noplot ,colour = 'grey'){


  rowIdsWithCov <-  (plpData$covariateData$covariates %>%
                       dplyr::filter(covariateId ==  covariateToStudy ) %>%
                       dplyr::select(rowId)%>%
                       dplyr::pull() )

  tempdata <- tempdata %>%
    dplyr::mutate(hasCovariate = 1 *(rowId %in% rowIdsWithCov))

  #force some random order
  randomIndex<- sample(noSyms*noPatientsInSym)
  tempdata <- tempdata[randomIndex, ]

  frequencies <- c()
  for(i in 0:(noSyms-1)){
    frequencies <- append(frequencies, mean(tempdata$hasCovariate[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)]))
  }

  toPlot <- data.frame(frequencies=frequencies,position= rep(noplot,noSyms )  )

  #return(toPlot)
  return( ggplot2::geom_boxplot(
    data = toPlot,
    mapping = ggplot2::aes( y=frequencies, x= position),
    colour = colour
  ))
}
