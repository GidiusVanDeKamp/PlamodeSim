#' New probablies for cox model
#'
#' @param plpModel a data set like the type used with plp
#' @param plpData a data set like the type used with plp
#'
#' @return returns the Breslow estimator
#' @export
#'
#'
newPropsCoxversion2 <- function(plpModel,
                                plpData){
  parameters<- plpModel$model$coefficients
  indexParamNonZero <- (parameters$betas != 0)%>%
                        which()
  BetaZ <- rep(0, max(plpData$cohorts$rowId) )

  for(i in 1:length(indexParamNonZero)){
    indexes <-    (plpData$covariateData$covariates %>%
                  dplyr::filter(.data$covariateId ==!! as.numeric(parameters[indexParamNonZero[i],2] ))%>%
                  dplyr::select( .data$rowId) %>%
                  dplyr::collect())$rowId

    BetaZ[indexes ] <-   BetaZ[indexes] + as.numeric(parameters[indexParamNonZero[i],1])
  }
  expBetaZ<- exp(BetaZ) #expBetaZ is orderd by rowId

  outcomesOrderd <- plpData$outcomes %>%
                    arrange( daysToEvent)
  uniqueTimes<- unique(outcomesOrderd$daysToEvent)

  breslow <-   rep(0, length(uniqueTimes))

  for(j in 2:length(uniqueTimes)){   #the censored data is not added
    indexWithoutRealisation <- which(outcomesOrderd$daysToEvent > uniqueTimes[j]) # switch outcomes orderd for the normal ones.
    breslow[j] <- breslow[j-1]+
                  (1/sum(expBetaZ[indexWithoutRealisation]))* #here the censored data should be included
                  sum(1*(outcomesOrderd$daysToEvent==uniqueTimes[j] )) #here the censored should be excluded
  }
  HatSzero <- exp(-breslow)

  #return(data.frame(HatSzero= HatSzero, uniqueTimes= uniqueTimes ))
  #we could make all the props

  props <- matrix(0, max(plpData$cohorts$rowId), length(uniqueTimes))
  for(k in 1:length(uniqueTimes)){
    props[,k]<- HatSzero[k]^expBetaZ
  }

  return(list(props,uniqueTimes))
}
