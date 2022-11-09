#'hello
#'
#' @param plpData the plpData
#' @param executeSettings the executeSettings
#' @param populationSettings the populationSettings
#' @param splitSettings the splitSettings
#' @param sampleSettings, the sample Settings
#' @param preprocessSettings, the preprocessSettings
#' @param featureEngineeringSettings the featureEngineeringSettings.
#' @param outcomeId the outcomeID
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


  population <- tryCatch({
    do.call(PatientLevelPrediction::createStudyPopulation, list(plpData = plpData,
                                        outcomeId = outcomeId, populationSettings = populationSettings,
                                        population = plpData$population))
  }, error = function(e) {
    ParallelLogger::logError(e)
    return(NULL)
  })
  # #addsome sort of trycatch?
  # population <- PatientLevelPrediction::createStudyPopulation(
  #   plpData = plpData,
  #   outcomeId = outcomeId,
  #   populationSettings = populationSettings
  # )

  data<- plpData
  # i am not sure what should happen when executesettings$runsplitdata = False
#
  if(executeSettings$runSplitData){
    # split the data (test/train/cv) + summarise at the end
    data <- tryCatch(
      {
        do.call(PatientLevelPrediction::splitData,list(plpData = data,
                                                       population = population,
                                                       splitSettings = splitSettings))
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


  return(data)
}
