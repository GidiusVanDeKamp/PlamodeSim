#' visual for a covariate
#'
#' @param plpData a data set like the type used in PatientLevelPredict
#' @param studyCovariateId a number, the covariateId of covariate to restrict to.
#' @param noSimulations number of simulations
#' @param noPersons number of persons in each simulation
#' @param parameters a data set with parameters like the type used for plp
#' @param modelName a string with the type of model.
#'
#' @return returns a histogram for the frequencies of the outcome the frequency is calculated with only the patients that have the given covariate Id present.
#' @export
#'
visualOutcomeCovariateId <- function(
                     plpData,
                     studyCovariateId,
                     noSimulations,
                     noPersons,
                     parameters,
                     modelName
                     ){

  plpModel<- makeModel(parameters, modelName)
  newprops<- PatientLevelPrediction::predictPlp(plpModel, plpData,plpData$cohorts)

  obsfreq<- c()
  for(i in 1:noSimulations){
    newout <- newOutcomes(noPersons ,newprops )
    obsfreq<- obsfreq %>%
              append(observedFrequency(newout,plpData ,studyCovariateId ))

  }
  obsfreq <- data.frame(freq= obsfreq)
  plotGreenLine <- greenLine(plpResult,
                             studyCovariateId,3)

  indexstudycov<- which(studyCovariateId==plpResult$covariateSummary$covariateId )

  ggplot2::ggplot(obsfreq, ggplot2::aes(freq))+
  ggplot2::geom_histogram(binwidth=0.05)+
  ggplot2::geom_vline(xintercept =plotGreenLine, col='green')+
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
greenLine <- function(plpResult,
                      studyCovariateId,
                      youroutcomeId){

  indexOutcome <- (plpData$outcomes %>%
                     dplyr::filter( outcomeId == youroutcomeId) %>% #change 3 to new variable called outcomeid
                     dplyr::select(rowId) )[[1]]

  indexesCovariate <- (plpData$covariateData$covariates %>%
                         dplyr::filter( covariateId == studyCovariateId) %>%
                         dplyr::select(rowId) %>%
                         dplyr::count()%>%
                         dplyr::collect( )%>%
                         as.numeric())[[1]]

  indexesCovariateAndOutcome <- (plpData$covariateData$covariates %>%
                                   dplyr::filter( covariateId == studyCovariateId & rowId %in% indexOutcome) %>%
                                   dplyr::select(rowId) %>%
                                   dplyr::count()%>%
                                   dplyr::collect( )%>%
                                   as.numeric())

  return(indexesCovariateAndOutcome/indexesCovariate)
}

