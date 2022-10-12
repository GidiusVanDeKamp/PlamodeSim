#' simulate a new outcome
#'
#' @param plpResult a data set as returned by newOutcomesParameters
#' @param plpData a data set as with covariates,
#' @param noSimulations number of simulations preformed
#' @param noPersons numner of person in each simulatons
#' @param parameters a data set with parameters
#'
#'
#' @return returns a histogram for the frequencies of the outcome
#' @export
#'
visualOutcomePlpResult <- function( plpResult,
                           plpData,
                           noSimulations,
                           noPersons,
                           parameters =plpResult$covariateSummary$covariateValue){

  newprops<- newPropsParametersPlpResults(plpResult , plpData, parameters)
  obsfreq<- c()
  for(i in 1:noSimulations){
    newout <- newOutcomes(noPersons ,newprops$newProps )
    obsfreq<- obsfreq %>%
      append(sum(newout$newOutcomes)/ noPersons)
  }
  obsfreq= data.frame('obsfreq'= obsfreq)

  part <- (plpData$outcomes %>%
          filter( outcomeId== 3) %>%
          dim())[1]
  total <- part + length(plpData$cohorts$targetId)
  plotGreenLine <-  part/total

  differentParams<- which(parameters!=plpResult$covariateSummary$covariateValue )
  redlines <- plogis(parameters[differentParams])

  #return(obsfreq)
   ggplot2::ggplot(obsfreq, ggplot2::aes(obsfreq))+
   ggplot2::geom_histogram(binwidth=0.05)+
   ggplot2::geom_vline(xintercept =plotGreenLine, col='green')+
   ggplot2::geom_vline(xintercept =redlines , col='red')+
   ggplot2::coord_cartesian(xlim=c(-0.1,1.1))+
   ggplot2::ggtitle(paste(
    "histogram of the frequency of the outcome for",
    noSimulations,
    "simulations with",
    noPersons,
    "persons."))

  }

