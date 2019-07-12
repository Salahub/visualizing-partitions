### Drawing Partitions from Graphical Objects ############################################

##' @title Drawing a Single Partition
##' @param partition a partition represented as a sequence of positive integers
##' @param type one of "circle" or "rectangle"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a plot of the partition
##' @author Chris Salahub
draw_part <- function(partition, type = c("circle","rectangle"), eps = 0.1,
                      params = gpar()) {
    grid.newpage() # create the plotting area
    rects <- part_coords(partition, type, eps, params) # create graphical object
    grid.draw(rects) # draw the object
}

##' @title Drawing All Partitions of n
##' @param n a positive integer to be partitioned and displayed
##' @param type one of "cicle" or "rectangle"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a plot of all partitions of n
##' @author Chris Salahub
