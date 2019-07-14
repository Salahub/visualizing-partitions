### Drawing Partitions from Graphical Objects ############################################

##' @title Drawing a Single Partition
##' @param partition a partition represented as a sequence of positive integers
##' @param type one of "circle" or "rectangle"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a plot of the partition
##' @author Chris Salahub
draw_part <- function(partition, type = c("circle","rectangle"), eps = 0.1,
                      params = gpar(), ...) {
    if (length(partition) == 1) {
        stopifnot(partition > 0, partition %% 1 == 0) # check that the partition is valid
    } 
    else {
        stopifnot(all(diff(partition) <= 0), all(partition %% 1 == 0), 
                  all(partition > 0)) # check that the partition is valid
    }
    grid.newpage() # create the plotting area
    rects <- part_coords(partition, type, eps, params, ...) # create graphical object
    grid.draw(rects) # draw the object
}

##' @title Drawing All Partitions of n
##' @param n a positive integer to be partitioned and displayed
##' @param type one of "cicle" or "rectangle"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a plot of all partitions of n
##' @author Chris Salahub
draw_all_parts <- function(n, type = c("circle","rectangle"), eps = 0.1,
                           params = gpar()) {
    ## perform some checks
    stopifnot(n >= 0,
              n %% 1 == 0) # must be positive integer
    ## generate important features
    allpart <- parts(n) # generate all partitions
    numpars <- ncol(parts) # get the number of partitions
    n.grid <- ceiling(sqrt(numpars)) # the number of boxes in x and y
    ## use n.grid to generate graphical settings
    coords <- expand.grid(seq(0, 1, length.out = n.grid), seq(0, 1, length.out = n.grid))
    width <- 1/n.grid
    ## generate viewports for the partitions being plotted
    vps <- lapply(1:numpars, function(ind) viewport(x = coords[ind,1], y = coords[ind,2],
                                                    width = width, height = width))
    ## use all of this to plot the partitions
    for (ii in 1:numpars) draw_part(allpart[,ii), type, eps, params, vp = vps[[ii]])
}
