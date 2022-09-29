#' plots the difference bewteen the total of outcomes in the original data set
#' and a data set simulated with the same length
#'
#' @param data orginal dataset
#' @param N number of simulations
#' @param parameter parameters used for generating the data
#'
#' @return returns a plot
#' @export
#'
plotDiffInTotalOutcomes<- function(data, N, parameter){
  DifNumOutcomes <- c()
  minsum <-  sum(data$outcome)
  for (i in 1:N){
    DifNumOutcomes[i] <- sum(NewGroupData(data,
                                          length(data[,1]),
                                          parameter )[,1]) - minsum
  }
  plot(1:N, DifNumOutcomes,
       main = "difference in the sum of outcomes between given and generated data")
}
