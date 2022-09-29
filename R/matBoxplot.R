#' Generates a random matrix.
#' This is usefull for plotting.
#'
#' @param Nofsim number of rows
#' @param Nofgroups number of colums
#' @param Nobserv the maximum number in the matrix
#'
#' @return a random matrix
#' @export
#'
matBoxplot<- function(Nofsim=10, Nofgroups=200, Nobserv=30510) {
  sample(1:Nobserv, Nofsim*Nofgroups, replace = T) %>%
    matrix( Nofgroups,Nofsim) %>%
    return()
}
