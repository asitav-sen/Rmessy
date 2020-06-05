

.onAttach <- function(juggling_jaguar, Rmessy) {
  packageStartupMessage(
    "Thanks for installing. The package is built for my personal convenience, but feel free to use and improve."
  )
}

#' A function to generate combination of defined number of columns from a given data frame and binds them in rows. Usefult to create files for network analysis
#' Takes two values - A data frame from which columns are to be selected and number of columns to select for each set
#' @param data.df a data frame of more than 2 columns
#' @param m an integer value. Number of columns to select for each set to create. Defaults to 2
#' @return a data frame with supplied number of column, which is created from the sets by binding
#' @example
#' df<-data.frame(a= c(1), b= c(2), c= c(3), d= d(4))
#' juggling_jaguar(df,m=3)
#' @export
#' @keywords combination, columns, network
# juggling jaguar juggles with columns. you need to give the data frame and number of columns to play with.
juggling_jaguar <- function(data.df, m = 2) {
  if (is.data.frame(data.df) == FALSE) {
    stop("error: data frame not supplied")
  }

  if (is.integer(m) == FALSE) {
    stop("error: supplied m is not integer")
  }

  #get number of columns
  n = ncol(data.df)

  if (n <= 2) {
    stop("error: number of columns less than or equal to 2")
  }

  if (m >= n) {
    stop("error: supplied m is greater than of equal to number of columns")
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
  return(new.data)

}
