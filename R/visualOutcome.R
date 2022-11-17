#' simulate a new outcome
#'
#' @param plpData a data set like the type used for plp
#' @param noSimulations number of simulations
#' @param noPersons number of persons in each simulation
#' @param parameters a data set with parameters like the type used for plp
#'
#'
#' @return returns a histogram for the frequencies of the outcome
#' @export
#'
#' @importFrom rlang .data
visualOutcome <- function( plpData,
                           noSimulations,
                           noPersons,
                           parameters ){

  plpModel <- makeLogisticModel(parameters)
  newprops <- PatientLevelPrediction::predictPlp(plpModel, plpData, plpData$cohorts)

  obsfreq<- c()
  for(i in 1:noSimulations){
    newout <- newOutcomes(noPersons ,newprops )
    obsfreq<- obsfreq %>%
      append(sum(newout$outcomeCount)/ noPersons)
  }
  obsfreq= data.frame('obsfreq'= obsfreq)

  part <- plpData$outcomes %>%
          dplyr::filter(.data$outcomeId== 3) %>%
          dplyr::count() %>%
          as.integer()

  total <- length(plpData$cohorts$targetId)
  plotGreenLine <-  part/total

  #redlines<- theoreticalExpectation(plpData, parameters)
   ggplot2::ggplot(obsfreq, ggplot2::aes(obsfreq))+
   ggplot2::geom_histogram(binwidth=0.025)+
   ggplot2::geom_vline(xintercept =plotGreenLine, col='green')+
  # ggplot2::geom_vline(xintercept =redlines , col='red')+
   ggplot2::coord_cartesian(xlim=c(-0.1,1.1))+
   ggplot2::ggtitle(paste(
    "histogram of the frequency of the outcome for",
    noSimulations,
    "simulations with",
    noPersons,
    "persons."))

  }

