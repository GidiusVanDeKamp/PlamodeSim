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
    Ndata <- NewData(data, parameter, Indexes[,i])
    Ndata <- cbind(Ndata, data[Ndata[[2]], ])
    A<- length(which(Ndata$NewOutcome==1 ))
    B<- length(which(Ndata$NewOutcome==0))
    if (A > 0){
      Norow<- nrow(Datframe1)+ 1:A
      Datframe1[Norow , 'out']<- rep(1,A)
      Datframe1[Norow , 'noofcov']<- Ndata[which(Ndata$NewOutcome==1 ),NoOfCov+2]
      #Datframe[Norow , 'sim']<- rep(paste("simulation",i),A)
      Datframe1[Norow , 'sim']<- rep(i,A)
    }
    if (B > 0 ){
      Norow<- nrow(Datframe2)+ 1:B
      Datframe2[Norow , 'out']<- rep(0,B)
      Datframe2[Norow , 'noofcov']<- Ndata[which(Ndata$NewOutcome==0 ),NoOfCov+2]
      #Datframe[Norow , 'sim']<- rep(paste("simulation",i),B)    }
      Datframe2[Norow , 'sim']<- rep(i,B)
    }
  }
  mycolor1<- colorRampPalette(c("red","yellow"))(ncol(Indexes))
  mycolor2<- colorRampPalette(c("green","blue"))(ncol(Indexes))

  ggplot() + geom_density(data= Datframe1, aes(noofcov, col= factor(sim)), alpha= 0.7,binwidth=2)+
    scale_colour_manual(values=mycolor1)+
    ggnewscale::new_scale_colour() +
    geom_density(data= Datframe2, aes(noofcov, col= factor(sim) ), alpha=0.7,binwidth=2)+
    scale_colour_manual(values=mycolor2)+
    xlab(paste("schale of the covariate",names(parameter)[NoOfCov]))+
    theme(legend.position = "none")+
    labs(title=  paste(names(parameter)[NoOfCov],"split by outcome, blue for no outcome, and red for outcome"))
}
