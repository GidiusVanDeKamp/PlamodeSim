#' returns the probability of an outcome under the parameters specified
#'
#' @param data orginal dataset
#' @param parameters  parameters
#' @param index optional: indexes for the elements u want the probability of
#'
#' @return returns a vector with probabilities
#' @export
#'
probsUnderModel <- function( data, parameters , index= 1:dim(data[1])[1]){
  OddsUnderModel <- (as.matrix(data[as.matrix(index),-1]) %*%
                       t(as.matrix(parameters[-1]))) + as.numeric(parameters[1])
  Probs <- plogis(OddsUnderModel)
  return(Probs)
}




