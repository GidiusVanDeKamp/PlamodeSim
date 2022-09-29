#' frequency for outcome in a specific group
#'
#' @param data orginal dataset
#' @param parameters parameters only used to obtain the names of the covariates. this should be different
#' @param NoOfCov numer/index of the covariate
#'
#' @return returns a dataframe with the frequencies of the outcome in the different groups
#' @export
#'
#'
freqInGroup <- function(data, parameters, NoOfCov){
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
