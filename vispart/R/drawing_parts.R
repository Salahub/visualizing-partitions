### Drawing Partitions from Graphical Objects ############################################

##' @title Drawing a Single Partition
##' @param partition a partition represented as a sequence of positive integers
##' @param type one of "circle" or "rect"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a plot of the partition
##' @author Chris Salahub
draw_part <- function(partition, type = "rect", eps = 0.1,
                      params = gpar(), ...) {
    grid.newpage() # create the plotting area
    grobs <- part_coords(partition, type, eps, params, ...) # create graphical object
    grid.draw(grobs) # draw the object
}

##' @title Drawing All Partitions of n
##' @param n a positive integer to be partitioned and displayed
##' @param type one of "cicle" or "rect"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a plot of all partitions of n
##' @author Chris Salahub
draw_all_parts <- function(n, type = "rect", eps = 0.1,
                           params = gpar()) {
    ## perform some checks
    stopifnot(n >= 0,
              n %% 1 == 0) # must be positive integer
    ## generate important features
    allpart <- parts(n) # generate all partitions
    numpars <- ncol(allpart) # get the number of partitions
    n.grid <- ceiling(sqrt(numpars)) # the number of boxes in x and y
    width <- 1/n.grid # width calculation
    ## use n.grid to generate coordinate matrix
    coords <- expand.grid(seq(0.5*width, 1-0.5*width, length.out = n.grid),
                          rev(seq(0.5*width, 1-0.5*width, length.out = n.grid)))
    ## generate viewports for the partitions being plotted
    vps <- lapply(1:numpars, function(ind) viewport(x = coords[ind,1], y = coords[ind,2],
                                                    width = width, height = width))
    ## make a new grid plot
    grid.newpage()
    ## use all of this to plot the partitions
    for (ii in 1:numpars) {
        pushViewport(vps[[ii]]) # go to correct coordinate scale
        grid.draw(part_coords(allpart[,ii], type, eps, params)) # draw the partition
        ## grid.text(label = ii, y = 0, hjust = 0.5, vjust = -0.1) # label
        popViewport() # return to global image
    }
}
