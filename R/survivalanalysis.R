#' Plot Matthews Correlation Coefficient (MCC) against cut off.
#' @param actual a vector of actual results from test set
#' @param predicted a vector of predicted results on test set
#' @return a plot with cutoff in x axis and MCC in y axis
#' @example
#'    \donttest{
#'     bestmccplot(c(1,0,1,0,1,1,0,0), c(0.2, 0.3, 0.4, 0.5, 0.6, 0.9, 0.3, 0.8))
#'       }
#' @export
#' @keywords confusion matrix evaluation, Matthews Correlation Coefficient


bestmccplot <- function(actual, predicted){

  #check if vectors
  if ( is.vector(actual) == FALSE | is.vector(predicted) == FALSE) {
    stop("error: vectors not supplied")
  }

  #check if equal length
  if(length(actual) != length(predicted)){
    stop("error: vectors not of equal length")
  }

  # check if null

  if(is.null(actual) | is.null(predicted)){
    stop("error: Null vector supplied")
  }


  cutoff<-vector(length = 100)
  mcc<-vector(length = 100)
  for(i in 1:100){
    cutoff[i]=i*0.01
    mcc[i]=ModelMetrics::mcc(
      actual = actual,
      predicted = predicted,
      cutoff = cutoff[i]
    )
  }
  p=plot(cutoff,mcc,type="l")
  return(p)
}


#' Returns the highest MCC among the all the MCCs calculated at cutoffs between 1% to 100% in sequence of 1% .
#' @param actual a vector of actual results from test set
#' @param predicted a vector of predicted results on test set
#' @return Highest MCC value among all MCC values plotted against cutoff values of 0.1 to 1
#' @example
#'    \donttest{
#'     bestmcc(c(1,0,1,0,1,1,0,0), c(0.2, 0.3, 0.4, 0.5, 0.6, 0.9, 0.3, 0.8))
#'       }
#' @export
#' @keywords confusion matrix evaluation, Matthews Correlation Coefficient


bestmcc <- function(actual, predicted){

  #check if vectors
  if ( is.vector(actual) == FALSE | is.vector(predicted) == FALSE) {
    stop("error: vectors not supplied")
  }

  #check if equal length
  if(length(actual) != length(predicted)){
    stop("error: vectors not of equal length")
  }

  # check if null

  if(is.null(actual) | is.null(predicted)){
    stop("error: Null vector supplied")
  }


  cutoff<-vector(length = 100)
  mcc<-vector(length = 100)
  for(i in 1:100){
    cutoff[i]=i*0.01
    print(cutoff)
    print(actual)
    print(predicted)
    mcc[i]=ModelMetrics::mcc(
      actual = actual,
      predicted = predicted,
      cutoff = cutoff[i]
    )
  }
  m<-mcc[which.max(mcc)]
  return(m)
}
