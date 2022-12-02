#' box plot of frequency in the outcomes
#'
#' @param OutcomeData the data you want visualised.
#' @param noSyms number of frequencies to be calculated
#' @param noPatientsInSym number of patients to calculate the frequency with.
#' @param plpData plpData used for finding the green dots
#' @param placeInPlot the place in the plot you want this part to be
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns a boxplot of the frequencies of of outcome for the whole dataset.
#'
#' @export
#'
#' @importFrom rlang .data
frequencyOutcomePlot <- function(OutcomeData, noSyms, noPatientsInSym, plpData, placeInPlot, colour = 'grey'){

  greendot <- nrow(plpData$outcomes)/ nrow(plpData$cohorts)
  frequencies <- c()

  # force some random order
  randomIndex<- sample(noSyms*noPatientsInSym)
  tempdata <- OutcomeData[randomIndex, ]

  for(i in 0:(noSyms-1)){
   frequencies <- append(frequencies, mean(tempdata$outcomeCount[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)]))
  }
  toPlot <- data.frame(frequencies=frequencies,position= rep(placeInPlot,noSyms )  )

  return( list(
    ggplot2::geom_boxplot(data = toPlot,
                          mapping = ggplot2::aes(y= frequencies, x= position),
                          colour = colour),
    ggplot2::geom_point( mapping = ggplot2::aes(position, frequency),
                         data= data.frame(frequency = greendot, position = placeInPlot),
                         colour =  'green')
  ))
}
