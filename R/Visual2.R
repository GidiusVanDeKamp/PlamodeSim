#' simulate a new outcome
#'
#' @param data a data set as returned by newOutcomesParameters
#'
#' @return returns a plot
#' @export
#'
Visual2 <- function( data, covariates, studyCovariateId ){
  empFreq= c()
  for(i in 1:max(data$simulationNumber)){

    indexOutcomeForSim <- (data %>%
    dplyr::filter( newOutcomes == 1, simulationNumber==i) %>%
    dplyr::select(subjectId))[[1]]

    indexForSim <- (data %>%
    dplyr::filter( simulationNumber==i) %>%
    dplyr::select(subjectId) %>%
    as.vector())[[1]]

    outcomeAndCovariate <- covariates$covariates %>%
      dplyr::group_by( hoi = (rowId %in% indexOutcomeForSim ) & (covariateId == studyCovariateId )) %>%
      dplyr::count()%>%
      dplyr::collect()

    hasCovariate <- covariates$covariates %>%
      dplyr::group_by(  hoi = (rowId %in% indexForSim ) & (covariateId == studyCovariateId )) %>%
      dplyr::count()%>%
      dplyr::collect()

    if(is.na(outcomeAndCovariate[2,2]) ){
      empFreq <- append(empFreq,0)
    }
    else {
      empFreq <- (outcomeAndCovariate[2,2]/hasCovariate[2,2]) %>%
      as.numeric() %>%
      append(x= empFreq,)
    }
  }
  return(empFreq)
  }

