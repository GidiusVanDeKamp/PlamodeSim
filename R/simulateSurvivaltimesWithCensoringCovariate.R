#' predict censoring times
#'
#' @param censorModel a Model as made by fitModelWithCensoring a list with twomodels named censorModel and outcomesModel
#' @param plpData the plpData
#' @param population the population to drawfrom
#' @param populationSettings the populationSettings
#' @param numberToSimulate number of people to simulate new outcomes for.
#' @param covariateToStudy the covariate Id that has to be present in the simulated data
#'
#' @return returns a data set with new outcomes or censoring times
#'
#' @export
#'
#'
#' @importFrom rlang .data

simulateSurvivaltimesWithCensoringCovariate <- function(censorModel,
                                                        plpData,
                                                        population,
                                                        populationSettings,
                                                        numberToSimulate,
                                                        covariateToStudy){

  rowIdsWithCovariate <- (plpData$covariateData$covariates %>%
                            dplyr::filter(covariateId == covariateToStudy  ) %>%
                            dplyr::collect())$rowId

  population <- population %>%
    dplyr::filter(rowId %in% rowIdsWithCovariate)

  modelCensor <- censorModel$censorModel
  modelOutcomes <- censorModel$outcomesModel


  predictionOutcome <- PatientLevelPrediction::predictPlp(
    plpModel   = modelOutcomes,
    plpData    = plpData,
    population = population,
    timepoint  = populationSettings$riskWindowEnd
  )

  predictionCensor <- PatientLevelPrediction::predictPlp(
    plpModel   = modelCensor,
    plpData    = plpData,
    population = population,
    timepoint  = populationSettings$riskWindowEnd
  )

  baselineSurvivalOutcome <-attr(predictionOutcome, "metaData")$baselineSurvival
  baselineSurvivalCensor <-attr(predictionCensor, "metaData")$baselineSurvival

  predictionOutcome <- predictionOutcome %>%
    dplyr::mutate(
      exp_lp = log(1 - .data$value) / log(baselineSurvivalOutcome)
    )

  predictionCensor <- predictionCensor %>%
    dplyr::mutate(
      exp_lp = log(1 - .data$value) / log(baselineSurvivalCensor)
    )

  # newOutcomesCensoredSurvivalTimes2(  # i think i should remake this function better.
  #   plpModelCensoring = modelCensor,
  #   expbetasCensor = predictionCensor$exp_lp ,
  #   plpModel = modelOutcomes,
  #   expbetas = predictionOutcome$exp_lp,
  #   numberToSimulate
  # )%>%


  baselineSurvOutcome <- modelOutcomes$model$baselineSurvival$surv
  baselineTimesOutcome <- modelOutcomes$model$baselineSurvival$time

  baselineSurvCensor <- modelCensor$model$baselineSurvival$surv
  baselineTimesCensor <- modelCensor$model$baselineSurvival$time

  index <- sample(rowIdsWithCovariate, numberToSimulate, replace=T)
  uniformSampleOutcome <- stats::runif(numberToSimulate)
  uniformSampleCensor <- stats::runif(numberToSimulate)

  toreturn<- data.frame(rowId= index)

  baselineTimesOutcome<- c(0,baselineTimesOutcome)
  baselineTimesCensor<- c(0,baselineTimesCensor)

  # i think the inf should be skipped/it is unnecessary

  for( i in 1:numberToSimulate){
    id <- index[i]
    expbetalpOutcome <-  (predictionOutcome %>%
                            dplyr::filter(.data$rowId == id) %>%
                            dplyr::select( exp_lp))$exp_lp
    expbetalpCensor <-  (predictionCensor %>%
                           dplyr::filter(.data$rowId == id) %>%
                           dplyr::select( exp_lp))$exp_lp

    propsOutcome <- baselineSurvOutcome^expbetalpOutcome
    propsCensor  <- baselineSurvCensor^expbetalpCensor

    toreturn$outcomeTime[i] <- baselineTimesOutcome[sum((propsOutcome>uniformSampleOutcome[i])*1)+1]
    toreturn$censorTime[i] <- baselineTimesCensor[sum((propsCensor>uniformSampleCensor[i])*1)+1]
  }

  toreturn%>%
    dplyr::mutate(survivalTime = pmin(
      outcomeTime,.data$censorTime),
      outcomeCount = ((.data$survivalTime==.data$outcomeTime)*1)*
        ((survivalTime < censorTime )*1)
    )%>%
    dplyr::select(.data$rowId, .data$survivalTime, .data$outcomeCount)%>%
    return()
}


