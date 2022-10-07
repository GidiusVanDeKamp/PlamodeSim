#' Makes a boxplot for the emperical frequency of the outcome.
#'
#' @param data Original dataset
#' @param parameter Parameters for the model used to generate the datasets
#' @param NoOfCov Number/place of the covariate u want study with the plot
#' @param Indexes Matrix with for echt colum a simulation and each row contains
#' the indexes of the pasients in the group.
#'
#' @return makes a plot
#' @export
#'
#'
boxplotFreq <- function( data, parameter, noOfCov, indexes ) {
  datFrame <- data.frame(nosim = 1:dim(indexes)[2])
  for (i in 1:dim(indexes)[2]) {
    Ndata <- newData(data, parameter,indexes[,i])
    Ndata <- cbind(Ndata, data[Ndata[[2]], ])
    for (j in 1:length(noOfCov)) {
      datFrame[i, paste(names(parameter)[noOfCov[j] ],
                        1:nrow(unique(data[noOfCov[j] ]))
                        ,sep = "")
      ] <-  (Ndata %>%
               dplyr::group_by(across(noOfCov[j] + 2)) %>%
               dplyr::summarise(freqSim = mean(outcome))
      )[2] %>%
        t()
    }
  }
  Dots<-  freqInGroup(data,param, noOfCov)
  ggplot2::ggplot(tidyr::pivot_longer(datFrame,-1))+
    ggplot2::geom_boxplot(ggplot2::aes(name, value))+
    ggplot2::geom_point(data= Dots, ggplot2::aes(name, value), col= "green")+
    ggplot2::labs(x= "facor", y= "frequency")
}
