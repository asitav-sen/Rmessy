
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
#'    \donttest{
#'     juggling_jaguar(data.frame(a= c(1), b= c(2), c= c(3), d= d(4)),m=3)
#'       }
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




# .onAttach <- function(mapping_monkey, Rmessy) {
#   packageStartupMessage(
#     "Thanks for installing. The package is built for my personal convenience, but feel free to use and improve."
#   )
# }

#' Calculates network parameters from graph file and returns list of results
#' @param graphitem an Igraph item
#' @param mode used to calculate degree. defaults to "all"
#' @param norm defaults to TRUE. Used to control the parameter "normalized" in betweenness
#' @param direc defaults to FALSE. Used to control parameter direction in eigen_centrality, betweenness, diameter, farthest_vertices, get_diameter and assortativity.degree
#' @param loops defaults to FALSe. Used to control loop parameter in edge_density
#' @return a list of results
#' @example
#'    \donttest{
#'      mapping_monkey(graphitem, mode="all",norm=TRUE, direc=FALSE, loops=FALSE)
#'    }
#' @export
#' @keywords network analysis
# mapping_monkey maps network properties and returns a list of result.
mapping_monkey<-function(graphitem, mode="all",norm=TRUE, direc=FALSE, loops=FALSE){
  if(is.igraph(graphitem)==F){
    stop("IGraph item not supplied")
  }

  netlist<-vector("list",8)

  #vertices

  #degree
  degree<-igraph::degree(graphitem, mode = "all")

  #betweenness

  betweenness<-igraph::betweenness(graphitem, normalized = norm, directed = direc)

  #eigen

  ei<-igraph::eigen_centrality(graphitem, directed = direc)
  eigenvector<-ei$vector

  #edges

  density<- igraph::edge_density(graphitem, loops = loops)
  diameter<- igraph::diameter(graphitem, directed = direc)
  farthest<- igraph::farthest_vertices(graphitem, directed = direc)
  diaroute<- igraph::get_diameter(graphitem, directed = direc)
  asrtvty<- igraph::assortativity.degree(graphitem, directed = direc)
  trnsvt<- igraph::transitivity(gra.main)

  netlist<-list(degree=degree, betweenness=betweenness, eigenvector=eigenvector,density=density, diameter=diameter, farthest=farthest, diaroute=diaroute,asrtvty=asrtvty,trnsvt=trnsvt)
  return(netlist)
}

