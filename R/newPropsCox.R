#' New probablies for cox model
#'
#' @param plpModel a data set like the type used with plp
#' @param plpData a data set like the type used with plp
#' @param timeInDays time in days
#'
#' @return returns the Breslow estimator
#' @export
#'
#'
newPropsCox <- function(plpModel,
                        plpData,
                        timeInDays){

  SHatNul <- breslowEstimator(plpData, timeInDays , plpModel$model$coefficients)
  NofPatients <- max(plpData$cohorts$rowId)
  BetaZ <- rep(0,NofPatients)

  indexParamNonZero <- (plpModel$model$coefficients$betas != 0) %>%
                        which()

  for(i in 1:length(indexParamNonZero)){
    indexes <- (plpData$covariateData$covariates %>%
                  dplyr::filter(covariateId ==!! as.numeric(plpModel$model$coefficients[indexParamNonZero[i],2] ))%>%
                  dplyr::select( rowId) %>%
                  dplyr::collect())$rowId

    BetaZ[indexes] <- BetaZ[indexes]+  as.numeric(plpModel$model$coefficients[indexParamNonZero[i],1])
  }

  props <- 1-((rep(SHatNul,NofPatients))^(exp(BetaZ)))

  return(data.frame(rowId= 1:NofPatients, value= props ))

}
