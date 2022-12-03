#' box plot of frequency in a covariate the outcomes
#'
#'
#' @param OutcomeData the data you want visualised.
#' @param noSyms number of frequencies to be calculated
#' @param noPatientsInSym number of patients to calculate the frequency with.
#' @param covariateToStudy a covariate id
#' @param plpData plpData used for finding the green dots
#' @param placeInPlot the place in the plot you want this part t
#' @param colour something that ggplot accepts as a colour.
#'
#' @return returns a boxplot of the frequencie of outcome when a specific covariates is present.
#'
#' @export
#'
#' @importFrom rlang .data
frequencyOutcomeCovariatePlot <- function( OutcomeData,
                                           noSyms,
                                           noPatientsInSym,
                                           covariateToStudy,
                                           plpData,
                                           placeInPlot,
                                           colour = 'grey'){


    rowIdsWithCov <-  (plpData$covariateData$covariates %>%
                         dplyr::filter(covariateId ==  covariateToStudy ) %>%
                         dplyr::select(rowId)%>%
                         dplyr::pull() )


    tempdata <- OutcomeData %>%
      dplyr::mutate(hasCov = 1*(rowId %in% rowIdsWithCov),
                    hasCovAndOutcome =  hasCov*outcomeCount)

    noCovariateOutcome <-  plpData$outcomes %>% dplyr::filter(rowId %in% rowIdsWithCov) %>% dplyr::count()
    greendot <- as.numeric(noCovariateOutcome)/length(rowIdsWithCov)

    #force some random order
    randomIndex<- sample(nrow(tempdata))
    tempdata <- tempdata[randomIndex, ]

    frequencies <- c()
    for(i in 0:(noSyms-1)){
      frequencies <- append(frequencies,
                            sum(tempdata$hasCovAndOutcome[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)])/
                            sum(tempdata$hasCov[(1+i*noPatientsInSym):((i+1)*noPatientsInSym)]))
    }
    toPlot <- data.frame(frequencies=frequencies,position= rep(placeInPlot,noSyms )  )

    return(list(
      ggplot2::geom_boxplot(data = toPlot,
                            mapping = ggplot2::aes( y=frequencies, x= position),
                            colour = colour),
      ggplot2::geom_point( mapping = ggplot2::aes(position, frequency),
                           data= data.frame(frequency = greendot, position = placeInPlot),
                           colour =  'green')
    ))
}

