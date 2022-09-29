#' usefull for visualizing
#'
#' @param Nofsim number of simulations/ number of columns
#' @param Nofgroups number of groups/ number of rows
#' @param Nobserv maximum value in the matrix
#'
#' @return a random matrix with identical columns
#' @export
#'
matBoxplot2<- function(Nofsim=10, Nofgroups=200, Nobserv=30510) {
  sample(1:Nobserv, Nofgroups, replace = T) %>%
    matrix( Nofgroups,Nofsim) %>%
    return()
}
