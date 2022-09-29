#' Makes a boxplot for the emperical frequency of the outcome.
#'
#' @param data original dataset
#' @param parameter parameters for the model used to generate the datasets
#' @param NoOfCov number/place of the covariate u want study with the plot
#' @param Indexes matrix with for echt colum a simulation and each row contains
#' the indexes of the pasients in the group.
#'
#' @return makes a plot
#' @export
#'
#'
boxPlotfreq <- function( data, parameter, NoOfCov, Indexes ) {
  DatFrame <- data.frame(nosim = 1:dim(Indexes)[2])
  for (i in 1:dim(Indexes)[2]) {
    Ndata <- NewData(data, parameter,Indexes[,i])
    Ndata <- cbind(Ndata, data[Ndata[[2]], ])
    for (j in 1:length(NoOfCov)) {
      DatFrame[i, paste(names(parameter)[NoOfCov[j] ],
                        1:dim(distinct(data[NoOfCov[j] ]))[1]
                        ,sep = "")
      ] <-  (Ndata %>%
               group_by(across(NoOfCov[j] + 2)) %>%
               summarise(freqSim = mean(outcome))
      )[2] %>%
        t()
    }
  }
  Dots<-  FreqInGroup(data,param, NoOfCov)
  ggplot(pivot_longer(DatFrame,-1))+
    geom_boxplot(aes(name, value))+
    geom_point(data= Dots, aes(name, value), col= "green")+
    labs(x= "facor", y= "frequency")
}
