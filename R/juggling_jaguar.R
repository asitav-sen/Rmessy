
.onAttach <- function(juggling_jaguar, Rmessy) {
  packageStartupMessage(
    "Thanks for installing. The package is built for my personal convenience, but feel free to use and improve."
  )
}

#' Generates combination of 2 columns from a data frame
#' @param data.df a data frame of more than 2 columns
#' @param rm.na is set to true by default. It removes any rows with NA
#' @param dedup is set to true by default. It removes an rows where values of both columns are equal
#' @return a data frame with supplied number of column, which is created from the sets by binding
#' @example
#' juggling_jaguar(data.frame(a= c(1), b= c(2), c= c(3), d= d(4)),m=3)
#' @export
#' @keywords network analysis
# juggling jaguar juggles with columns. you need to give the data frame and number of columns to play with.
juggling_jaguar <- function(data.df, rm.na = TRUE, dedup= TRUE) {
  if (is.data.frame(data.df) == FALSE) {
    stop("error: data frame not supplied")
  }
  #get number of columns
  n = ncol(data.df)
  m = 2
  if (n <= 2) {
    stop("error: number of columns less than or equal to 2")
  }

  if (m >= n) {
    stop("error: supplied data frame does not have enough number of columns")
  }

  #calculate number of combinations
  k = choose(n, m)

  #create an empty list
  cdata = list()
  cdata = NULL

  #create data frames selecting 'm' columns and store in a lis
  for (i in 1:k) {
    l = 1
    for (j in 1:n) {
      for (o in j:n) {
        if (o != j) {
          cdata[[l]] <- data.frame(data.df[, j], data.df[, o])
          l = l + 1
        }
      }
    }
  }
  #bind the data frames in the list to create a new data frame
  new.data <- data.table::rbindlist(cdata, use.names = FALSE)
  names(new.data)[1]<-"x"
  names(new.data)[2]<-"y"

  # removing na
  if (rm.na == TRUE) {
    new.data <- stats::na.omit(new.data)
  }

  # dedup
  if(dedup == TRUE){
    new.data<-filter(new.data,x!=y)
  }

  return(new.data)

}
