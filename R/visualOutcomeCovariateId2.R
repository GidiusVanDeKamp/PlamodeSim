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
visualOutcomeCovariateId2 <- function(
                     plpData,
                     restrictToCovariateId,
                     noSimulations,
                     noPersons,
                     parameters,
                     modelName
                     ){
  # we will generate new outcome only from persons who have a specific covariate
  plpModel<- makeModel(parameters,modelName)
  newprops<- PatientLevelPrediction::predictPlp(plpModel, plpData,plpData$cohorts)

  #newprops<- newPropsParameters(plpData, parameters, modelName)
  obsfreq<- c()
  indexesWithCovariate <- (plpData$covariateData$covariates %>%
                          filter(covariateId == restrictToCovariateId)%>%
                          collect())$rowId

  for(i in 1:noSimulations){
    index <-  sample(indexesWithCovariate, noPersons, replace=T)
    newoutcomes <- stats::rbinom(noPersons, 1, newprops[index,'value'])

    newout <- data.frame(rowId = index, newOutcomes= newoutcomes )
    obs <- sum(newoutcomes)/noPersons
    obsfreq <- obsfreq %>%
               append(obs)
              #append(observedFrequency(newout,plpData ,restrictToCovariateId ))

  }
  obsfreq <- data.frame(freq= obsfreq)
  plotGreenLine <- greenLine(plpResult,
                             restrictToCovariateId,3)

  indexstudycov<- which(restrictToCovariateId==plpResult$covariateSummary$covariateId )


  ggplot2::ggplot(obsfreq, ggplot2::aes(freq))+
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
