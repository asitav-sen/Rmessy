
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rmessy

<!-- badges: start -->

<!-- badges: end -->

The goal of Rmessy is to Help lazy analysts to save some effort and
time.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asitav-sen/Rmessy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Rmessy)
#> Thanks for installing. The package is built for my personal convenience, but feel free to use and improve.
a<-c("A","a")
b<-c("B","b")
c<-c("C","c")
d<-c("D","d")

df<-data.frame(a,b,c,d)

juggling_jaguar(df)
#>     data.df...j. data.df...o.
#>  1:            A            B
#>  2:            a            b
#>  3:            A            C
#>  4:            a            c
#>  5:            A            D
#>  6:            a            d
#>  7:            B            C
#>  8:            b            c
#>  9:            B            D
#> 10:            b            d
#> 11:            C            D
#> 12:            c            d
```

The function is useful to build files for network analysis which takes
relationship values in two columns. However, in real life, data usually
don’t come in that way. Read the ‘Introduction’ for further clarity.
