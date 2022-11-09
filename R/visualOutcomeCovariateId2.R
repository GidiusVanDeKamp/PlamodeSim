#'visual for a covariate
#'
#' @param plpData a data set like the type used in PatientLevelPredict
#' @param restrictToCovariateId a number, the covariateId of covariate to restrict to.
#' @param noSimulations number of simulations
#' @param noPersons number of persons in each simulation
#' @param parameters a data set with parameters like the type used for plp
#'
#' @return returns a histogram for the frequencies of the outcome there is only simulated from patients that have the given covariate Id.
#' @export
#'
#' @importFrom rlang .data
visualOutcomeCovariateId2 <- function(
                     plpData,
                     restrictToCovariateId,
                     noSimulations,
                     noPersons,
                     parameters
                     ){
  plpModel<- makeLogisticModel(parameters)
  newprops<- PatientLevelPrediction::predictPlp(plpModel, plpData,plpData$cohorts)

  obsfreq<- c()
  indexesWithCovariate <- (plpData$covariateData$covariates %>%
                          dplyr::filter(.data$covariateId == restrictToCovariateId)%>%
                          dplyr::collect())$rowId

  for(i in 1:noSimulations){
    index <-  sample(indexesWithCovariate, noPersons, replace=T)
    newoutcomes <- stats::rbinom(noPersons, 1, newprops[index,'value'])

    newout <- data.frame(rowId = index, newOutcomes= newoutcomes )
    obs <- sum(newoutcomes)/noPersons
    obsfreq <- obsfreq %>%
               append(obs)

  }
  obsfreq <- data.frame(freq = obsfreq)
  plotGreenLine <- greenLine(plpData,
                             restrictToCovariateId,3)

  #indexstudycov<- which(restrictToCovariateId== plpResult$covariateSummary$covariateId )


  ggplot2::ggplot(obsfreq, ggplot2::aes(.data$freq))+
  ggplot2::geom_histogram(binwidth=0.05)+
  ggplot2::geom_vline(xintercept =plotGreenLine, col='green')+
  ggplot2::coord_cartesian(xlim=c(-0.1,1.1))+
  ggplot2::ggtitle(paste(
    "histogram of the frequency when outcome",
    restrictToCovariateId,
    "is present, for ",
    noSimulations,
    "sims with",
    noPersons,
    "persons."))

  }
