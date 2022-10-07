#' simulate a new outcome
#'
#' @param plpResult a runPlp
#' @param noSimulations number of dataset one wants
#' @param noPersons number of persons in each data set
#'
#'
#' @return returns a dataframe with newOutcomes and subjectId
#' @export
#'
newOutcomes<- function( plpResult, noSimulations, noPersons ){
  props <-  plpResult$prediction$value

  index <-  sample(1:length(props), noPersons*noSimulations, replace=T)
  newOutcomes <- stats::rbinom(noPersons,1, props[index])
  simulation <- c()
  for(i in 1:noSimulations){
    simulation<- append(simulation, rep(i, noPersons))
  }
  #return(simulation)
  return(data.frame(subjectId = index  ,newOutcomes= newOutcomes, simulationNumber = simulation  )  )
}
