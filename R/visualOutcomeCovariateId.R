#' simulate a new outcome
#'
#' @param plpData a data set like the type used for plp
#' @param plpResult a data set returned by plp
#' @param noPersons number of persons
#' @param noSimulations number of simulations
#' @param studyCovariateId a number, the covariateId of the to study covariate
#' @param parameters a data set like the type used for plp
#'
#' @return returns a histogram for the frequencies of the outcome
#' @export
#'
visualOutcomeCovariateId <- function( plpResult,
                     plpData,
                     studyCovariateId,
                     noSimulations,
                     noPersons,
                     parameters =plpResult$covariateSummary$covariateValue){

  newprops<- newPropsParametersPlpResults(plpResult , plpData, parameters)
  obsfreq<- c()
  for(i in 1:noSimulations){
    newout <- newOutcomes(noPersons ,newprops$newProps )
    obsfreq<- obsfreq %>%
              append(observedFrequency(newout,plpData ,studyCovariateId ))
  }
  obsfreq <- data.frame(freq= obsfreq)

  plotGreenLine <- greenLine(plpResult, studyCovariateId)

  indexstudycov<- which(studyCovariateId==plpResult$covariateSummary$covariateId )
  redLines <- plogis(parameters[indexstudycov])
  #return(redLines)

  ggplot2::ggplot(obsfreq, ggplot2::aes(freq))+
  ggplot2::geom_histogram(binwidth=0.05)+
  ggplot2::geom_vline(xintercept =plotGreenLine, col='green')+
  ggplot2::geom_vline(xintercept =redLines, col='red')+
  ggplot2::coord_cartesian(xlim=c(-0.1,1.1))+
  ggplot2::ggtitle(paste(
    "histogram of the frequency when outcome",
    studyCovariateId,
    "is present, for ",
    noSimulations,
    "sims with",
    noPersons,
    "persons."))

  }
greenLine <- function( plpResult,
                       studyCovariateId){

  indexOutcome <- (plpData$outcomes %>%
                     #dplyr::filter( outcomeId == youroutcomeId) %>% #change 3 to new variable called outcomeid
                     dplyr::select(rowId) )[[1]]

  indexesCovariate <- (plpData$covariateData$covariates %>%
                         dplyr::filter( covariateId == studyCovariateId) %>%
                         dplyr::select(rowId) %>%
                         dplyr::count()%>%
                         dplyr::collect( )%>%
                         as.vector())[[1]]

  indexesCovariateAndOutcome <- (plpData$covariateData$covariates %>%
                                   dplyr::filter( covariateId == studyCovariateId & rowId %in% indexOutcome) %>%
                                   dplyr::select(rowId) %>%
                                   dplyr::count()%>%
                                   dplyr::collect( )%>%
                                   as.vector())[[1]]

  return(indexesCovariateAndOutcome/indexesCovariate)
}

