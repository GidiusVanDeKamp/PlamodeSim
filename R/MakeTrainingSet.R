#'hello
#'
#' @param prediction form a predictPlp
#' @param baselineSurvival
#'
#' @return returns the prediction with an extra column
#' @export
#'
MakeTraingSet <- function(plpData,
                          executeSettings,
                          populationSettings,
                          splitSettings,
                          sampleSettings,
                          preprocessSettings,
                          featureEngineeringSettings,
                          outcomeId){

  population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                      outcomeId = outcomeId,
                                      populationSettings = populationSettings)

  #### if executeSettings ####
  if(executeSettings$runSplitData){
    # split the data (test/train/cv) + summarise at the end
    data <- tryCatch(
      {
        PatientLevelPrediction::splitData(
          plpData = plpData,
          population = population,
          splitSettings = splitSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data)){
      stop('data NULL after splitting')
    }

    PatientLevelPrediction:::dataSummary(data)
  }

  if(executeSettings$runSampleData){
    # sampling
    data$Train <- tryCatch(
      {
        PatientLevelPrediction:::sampleData(
          trainData = data$Train,
          sampleSettings = sampleSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train)){
      stop('train data NULL after sample')
    }
    PatientLevelPrediction:::dataSummary(data)
  }

  if(executeSettings$runfeatureEngineering){
    data$Train <- tryCatch(
      {
        PatientLevelPrediction:::featureEngineer(
          data = data$Train,
          featureEngineeringSettings = featureEngineeringSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train)){
      stop('train data NULL after feature engineering')
    }
    PatientLevelPrediction:::dataSummary(data)
  }

  if(executeSettings$runPreprocessData){

    data$Train$covariateData <- tryCatch(
      {
        PatientLevelPrediction:::preprocessData(
          covariateData = data$Train$covariateData,
          preprocessSettings = preprocessSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train$covariateData)){
      stop('train data NULL after preprocessing')
    }
    PatientLevelPrediction:::dataSummary(data)
  }
  #### ####

  return(data)
}
