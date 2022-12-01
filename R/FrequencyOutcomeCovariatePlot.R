#' box plot of frequency in a covariate the outcomes
#'
#' @param Data dataframe
#' @param covariateToStudy a covaraite id
#' @param plpData the covariate Data
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns a boxplot of the frequencie of outcome when a specific covariates is present.
#'
#' @export
#'
#' @importFrom rlang .data
frequencyOutcomeCovariatePlot <- function( tempdata,noPatientsInSym,noSyms, covariateToStudy, plpData,noplot, colour = 'grey'){
# function(censorModel, plpData, population, populationSettings, noSyms, noPatientsInSym, noplot, colour = 'grey'){


    rowIdsWithCov <-  (plpData$covariateData$covariates %>%
                         dplyr::filter(covariateId ==  covariateToStudy ) %>%
                         dplyr::select(rowId)%>%
                         dplyr::pull() )

    tempdata <- tempdata %>%
      dplyr::mutate(hasCov = 1*(rowId %in% rowIdsWithCov),
                    hasCovAndOutcome =  hasCov*outcomeCount)

    #force some random order
    randomIndex<- sample(nrow(tempdata))
    tempdata <- tempdata[randomIndex, ]

    #return(tempdata$hascovAndOutcome[(1+0*noPatientsInSym):((0+1)*noPatientsInSym)])
    frequencies <- c()
    for(i in 0:(noSyms-1)){
      frequencies <- append(frequencies,
                            sum(tempdata$hasCovAndOutcome[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)])/
                            sum(tempdata$hasCov[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)]))
    }
    toPlot <- data.frame(frequencies=frequencies,position= rep(noplot,noSyms )  )
  #return(toPlot)

    return( ggplot2::geom_boxplot(
      data = toPlot,
      mapping = ggplot2::aes( y=frequencies, x= position),
      colour = colour
    ))
  }

