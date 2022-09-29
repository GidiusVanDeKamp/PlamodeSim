# first version of the outline for the package

# ----
#set up move to Gusto setup

#cheack names
library(tidyverse)
load("gusto.rda")

gusto <- gusto %>%
  filter(tx != "SK+tPA") %>%
  rename(
    "outcome" = "day30",
    "treatment" = "tpa"
  )

gusto<- gusto %>%
  mutate( (miloc == 'Anterior')*1,(miloc == 'Inferior')*1,(miloc == 'Other')*1,
          (pmi== 'yes')*1) %>%
  rename( 'milocAnterior' = '(miloc == "Anterior") * 1',
          'milocInferior' = '(miloc == "Inferior") * 1',
          'milocOther' = '(miloc == "Other") * 1', 'Pmi'= '(pmi == "yes") * 1')

gusto$Killip <- as.integer(gusto$Killip)

Gusto <- data.frame(outcome = gusto$outcome, treatment = gusto$treatment,
                   age = gusto$age, Killip = gusto$Killip, sysbp = gusto$sysbp,
                   Pmi = gusto$Pmi, pulse = gusto$pulse,
                   milocAnterior = gusto$milocAnterior,
                   milocInferior = gusto$milocInferior )

param<- data.frame(Intercept = -7.729, treatment = -0.2098, age = 0.07703,
                   Killip = 0.6813, sysbp=-0.01787, Pmi = 0.4323,
                   pulse = 0.01857, milocAnterior = 0.2457,
                   milocInferior = -0.2698)

# ----
#functions

ProbsUnderModel <- function( data, parameters , index= 1:dim(data[1])[1]){
  OddsUnderModel <- (as.matrix(data[as.matrix(index),-1]) %*%
                       t(as.matrix(parameters[-1]))) + as.numeric(parameters[1])
  Probs <- plogis(OddsUnderModel)
  return(Probs)
}

#new Groupwith new outcome
NewGroupData <- function( data, N, parameter ){
  index <-  sample(1:length(data[,1]), N, replace=T)
  ProbModel <- ProbsUnderModel( data , parameter, index)
  data.frame(NewOutcome =rbinom(N ,1, ProbModel),ind= index)%>%
    return()
}

PlotDiffInTotalOutcomes<- function(data, N, parameter){
  DifNumOutcomes <- c()
  minsum <-  sum(data$outcome)
  for (i in 1:N){
    DifNumOutcomes[i] <- sum(NewGroupData(data,
                                          length(data[,1]),
                                          parameter )[,1]) - minsum
  }
  plot(1:N, DifNumOutcomes,
       main = "difference in the sum of outcomes between given and generated data")
}

FreqInGroup <- function(data, parameters, NoOfCov){
  vecname <- c()
  vecvalue <- c()
  for (j in 1:length(NoOfCov)) {
    vecname <-  append(vecname, paste(names(parameters)[NoOfCov[j] ],
                                      1:dim(distinct(data[NoOfCov[j]]))[1]
                                      ,sep = ""))
    vecvalue <-append(vecvalue, (
        data %>%
        group_by(across(NoOfCov[j])) %>%
        summarise(freq = mean(outcome)))[2] %>%
        t() )
  }
  dat <- data.frame(name = vecname, value = vecvalue)
  return(dat)
}

#new outcome for specified group
NewData <- function( data, parameter, Indexes ){
  ProbModel <- ProbsUnderModel( data , parameter, Indexes )
  data.frame(NewOutcome =rbinom(length(Indexes), 1, ProbModel),ind= Indexes) %>%
    return()
}

MatBoxplot<- function(Nofsim=10, Nofgroups=200, Nobserv=30510) {
  sample(1:Nobserv, Nofsim*Nofgroups, replace = T) %>%
    matrix( Nofgroups,Nofsim) %>%
    return()
}

MatBoxplot2<- function(Nofsim=10, Nofgroups=200, Nobserv=30510) {
  sample(1:Nobserv, Nofgroups, replace = T) %>%
    matrix( Nofgroups,Nofsim) %>%
    return()
}

BoxPlotfreq <- function( data, parameter, NoOfCov, Indexes ) {
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

#add the line fot the whole gustoline set. some thicker
EmpOutcome <- function( data, parameter, NoOfCov, Indexes ){
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
