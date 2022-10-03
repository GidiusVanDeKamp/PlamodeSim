#add the line fot the whole gustoline set. some thicker
#' Title
#'
#' @param data original dataset
#' @param parameter parameters used for genereting new outcomes
#' @param NoOfCov number/index of the covariate to study
#' @param Indexes matrix with the amount of colums for the simulations, and on the rows the indexes of pasients
#'
#' @return make a plot
#' @export
#'
empOutcome <- function( data, parameter, NoOfCov, Indexes ){
  Datframe1 <- data.frame()
  Datframe2 <- data.frame()
  for (i in 1:dim(Indexes)[2]) {
    Ndata <- newData(data, parameter, Indexes[,i])
    Ndata <- cbind(Ndata, data[Ndata[[2]], ])
    A<- length(which(Ndata$newOutcome==1 ))
    B<- length(which(Ndata$newOutcome==0))
    if (A > 0){
      Norow<- nrow(Datframe1)+ 1:A
      Datframe1[Norow , 'out']<- rep(1,A)
      Datframe1[Norow , 'noofcov']<- Ndata[which(Ndata$newOutcome==1 ),NoOfCov+2]
      #Datframe[Norow , 'sim']<- rep(paste("simulation",i),A)
      Datframe1[Norow , 'sim']<- rep(i,A)
    }
    if (B > 0 ){
      Norow<- nrow(Datframe2)+ 1:B
      Datframe2[Norow , 'out']<- rep(0,B)
      Datframe2[Norow , 'noofcov']<- Ndata[which(Ndata$newOutcome==0 ),NoOfCov+2]
      #Datframe[Norow , 'sim']<- rep(paste("simulation",i),B)    }
      Datframe2[Norow , 'sim']<- rep(i,B)
    }
  }
  mycolor1<- colorRampPalette(c("red","yellow"))(ncol(Indexes))
  mycolor2<- colorRampPalette(c("green","blue"))(ncol(Indexes))

  ggplot2::ggplot() +
    ggplot2::geom_density(data= Datframe1, ggplot2::aes( .data$noofcov, col= factor(sim)), alpha= 0.7) +
    ggplot2::scale_colour_manual(values=mycolor1) +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_density(data= Datframe2, ggplot2::aes(.data$noofcov, col= factor(sim) ), alpha=0.7 ) +
    ggplot2::scale_colour_manual(values=mycolor2) +
    ggplot2::xlab(paste("schale of the covariate",names(parameter)[NoOfCov]))+
    ggplot2::geom_density(data=data, ggplot2::aes(.data[[names(data)[NoOfCov]]],col= .data[[names(data)[1]]]),size=0.8)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::labs(title=  paste(names(parameter)[NoOfCov],"split by outcome, blue for no outcome, and red for outcome"))
}
