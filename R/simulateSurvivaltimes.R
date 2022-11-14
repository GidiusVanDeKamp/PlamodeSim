#'simulating of a survival time
#'
#' @param plpModel a plpModel
#' @param plpData a plpdata
#' @param numberToSimulate number of people to draw and simulate new outcomes for.
#' @param population the population to draw from
#' @param populationSettings the populationSettings
#'
#'
#'
#' @return returns a data set with new outcomes
#' @export
#'
#'
#' @importFrom rlang .data

simulateSurvivaltimes <- function(plpModel,
                                  plpData,
                                  numberToSimulate,
                                  population,
                                  populationSettings){ # population.

  predictionOutcome <- PatientLevelPrediction::predictPlp(
    plpModel   = plpModel,
    plpData    = plpData,
    population = population,
    timepoint  = populationSettings$riskWindowEnd
  )

  baselineSurvivalOutcome <-attr(predictionOutcome, "metaData")$baselineSurvival

  predictionOutcome <- predictionOutcome %>%
    dplyr::mutate(
      exp_lp = log(1 - .data$value) / log(baselineSurvivalOutcome)
    )
  # now we have what we need to simulate

  baselineSurv <- plpModel$model$baselineSurvival$surv #not used
  baselineTimes <- plpModel$model$baselineSurvival$time

  index <-  sample(predictionOutcome$rowId, numberToSimulate, replace=T)
  uniformSample <- stats::runif(numberToSimulate)

  #props<- matrix(0,numberToSimulate,length(baselineSurv))

  toreturn<- data.frame(.data$rowId= index)

  baselineTimes<- c(0,baselineTimes)
  # i think the inf should be skipped/it is unnecessary

  for( i in 1:numberToSimulate){
    id <- index[i]
    expbetalp <-  (predictionOutcome %>%
                     dplyr::filter(.data$rowId == id) %>%
                     dplyr::select( .data$exp_lp))$exp_lp

    props<- baselineSurv^expbetalp
    toreturn$outcome[i] <- baselineTimes[sum((props>uniformSample[i])*1)+1]

   }

    return(toreturn)

}
