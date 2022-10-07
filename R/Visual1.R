#' simulate a new outcome
#'
#' @param data a data set as returned by newOutcomesParameters
#'
#' @return returns a boxplot for the frequencies of the outcome
#' @export
#'
Visual1 <- function( plpResult,
                     covariates,
                     studyCovariateId,
                     noSimulations,
                     noPersons,
                     parameters =plpResult$covariateSummary$covariateValue){

  plotGreenLine <- greenLine(plpResult, covariates, studyCovariateId)
  newout <- newOutcomesParameters( plpResult, covariates, noSimulations, noPersons, parameters )
  obsfreq <- data.frame(freq= observedFrequencies( newout, covariates, studyCovariateId ))

  ggplot2::ggplot(obsfreq, ggplot2::aes(freq))+
  ggplot2::geom_histogram(binwidth=0.05)+
  ggplot2::geom_vline(xintercept =plotGreenLine, col='green')+
  ggplot2::coord_cartesian(xlim=c(-0.1,1.1))

  }

