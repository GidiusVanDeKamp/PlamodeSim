#' box plot of frequency in the outcomes
#'
#' @param Data dataframe
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns a boxplot of the frequencies of of outcome for the whole dataset.
#'
#' @export
#'
#' @importFrom rlang .data
frequencyOutcomePlot <- function(tempdata, noSyms, noPatientsInSym, noplot, colour = 'grey'){

  frequencies <- c()

  # force some random order
  randomIndex<- sample(noSyms*noPatientsInSym)
  tempdata <- tempdata[randomIndex, ]

  for(i in 0:(noSyms-1)){
   frequencies <- append(frequencies, mean(tempdata$outcomeCount[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)]))
  }
  toPlot <- data.frame(frequencies=frequencies,position= rep(noplot,noSyms )  )

  return( ggplot2::geom_boxplot(
    data = toPlot,
    mapping = ggplot2::aes( y=frequencies, x= position),
    colour = colour
  ))
}
