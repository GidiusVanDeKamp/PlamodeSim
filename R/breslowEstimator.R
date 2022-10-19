#'
#' @param plpData a data set like the type used for plp
#' @param timeInDays time in days
#' @param parameters a data set like the type used for plp
#'
#'
#' @return returns the Breslow estimator
#' @export
#'
#'
breslowEstimator <- function(plpData,   # how will i cheack this function?
                             timeInDays,
                             parameters
                                  ){

  setPatientsAtRisk <- plpData$outcomes %>%
                      dplyr::filter(daysToEvent <= timeInDays )

  RxjDay <- min(setPatientsAtRisk$daysToEvent)

  #make first the exp beta Z for all in the rowId then later use these in the sums of sums.
  indexParamNonZero <- (parameters$betas != 0)%>%
                        which()
  BetaZ <- rep(0,max(plpData$cohorts$rowId) )

  for(i in 2:length(indexParamNonZero)){
    indexes <- (plpData$covariateData$covariates %>%
               filter(covariateId ==!! as.numeric(parameters[indexParamNonZero[i],2] ))%>%
                select( rowId) %>%
                collect())$rowId

    BetaZ[indexes ] <-   BetaZ[indexes] + as.numeric(parameters[indexParamNonZero[i],1])
  }
  expBetaZ<- exp(BetaZ)

  Estimator <- 0
  while(RxjDay < timeInDays){ #is this < correct? or <=??

    RxjDay <- setPatientsAtRisk %>%
              filter( daysToEvent > RxjDay) %>%
              select( daysToEvent ) %>%
              min()
    sumexpId <- (plpData$outcomes %>%
                dplyr::filter(daysToEvent > RxjDay) %>%
                select(rowId))$rowId

    #return(sumexpId)
    Estimator <- Estimator+ 1/(sum(expBetaZ[sumexpId]))
  }

  return(exp(-Estimator))

}
