### The Draw Part Function ###############################################################

##' @title Drawing a Single Partition
##' @param partition a partition of a number represented as a sequence of positive integers
##' @param type one of "circle" or "rectangle"
##' @return a graphical object and a plot of the partition
##' @author Chris Salahub
draw_partition <- function(n, type = c("circle","rectangle")) {
    stopifnot(all(n >= 0)) # check the partition provided is valid
}

              
