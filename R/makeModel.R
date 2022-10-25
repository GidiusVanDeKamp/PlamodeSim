#' makes an not fitted model
#'
#'
#' @param props a vector with the probabilities order by rowId
#' @param noPersons number of persons in the returned data set
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
makeModel<- function( parameters , modelname = "logistic"){
  covariateImportance <- ""
  trainDetails  <-""
  modelDesign  <- ""
  covariateId <- (parameters[-1,"covariateIds"])

  normFactors <- data.frame(covariateId = covariateId, maxValue = rep(1,length(parameters[1])))
  preprocessing  <- list(featureEngineering= NULL,tidyCovariates= list(normFactors=normFactors ) ) # cannot be zero
  model <- list(modelType= modelname, coefficients= parameters)

  mademod <- list(covariateImportance= covariateImportance,
                  trainDetails= trainDetails,
                  modelDesign= modelDesign,
                  preprocessing= preprocessing,
                  model= model
  )

  # what do these mean? i just added them so it will work.
  attr(mademod, "predictionFunction")<- "predictCyclops"
  attr(mademod, "modelType")<- "binary"

  return(mademod)
}
