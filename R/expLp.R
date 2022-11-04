#' hello
#'
#' @param plpData a data set like the type used with plp
#' @param plpModel a data set like the type used with plp
#'
#'
#' @return returns the expBetaz orderd by rowId
#' @export
#'
#' @importFrom rlang .data
expLp <- function(plpData,
                  plpModel){

  parameters <- plpModel$model$coefficients
  indexParamNonZero <- (parameters$betas != 0)%>%
                        which()
  BetaZ <- rep(0, max(plpData$cohorts$rowId) )

  for(i in 1:length(indexParamNonZero)){
    indexes <- (plpData$covariateData$covariates %>%
                dplyr::filter(.data$covariateId ==!! as.numeric(parameters[indexParamNonZero[i],2] ))%>%
                dplyr::select( .data$rowId) %>%
                dplyr::collect())$rowId

    BetaZ[indexes ] <-   BetaZ[indexes] + as.numeric(parameters[indexParamNonZero[i],1])
  }
  expBetaZ<- exp(BetaZ)

  return(expBetaZ)
}
