### Generating Partition Graphical Objects ###############################################
library(grid)

##' @title Generating Partition Graphical Objects
##' @param partition a partition represented as a sequence of positive integers
##' @param type one of "circle" or "rectangle"
##' @param eps a positive real number
##' @param coloursq a logical indicating whether the to colour the largest square
##' @param params a list of graphical parameters to be applied to the output
##' @return a graphical object corresponding to a plot of the partition
##' @author Chris Salahub and Pavel Schuldiner
part_coords <- function(partition, type = "rect", eps = 0.1,
                        coloursq = FALSE, params = gpar(), ...) {
    n <- sum(partition) # the value being partitioned, needed for checks and calculations
    ## perform checks
    stopifnot(all(partition >= 0),
              all(partition %% 1 == 0)) # check the partition provided is valid
    if (length(partition) < n) partition <- c(partition, rep(0, n - length(partition))) ## nice input handling
    ## calculate useful constants
    eps. <- eps*1/n # scale the eps value appropriately
    offset <- 1/(2*n) # the offset to place the centres correctly
    width <- unit((1-2*eps.)/n, units = "snpc") # the square width
    rad <- unit((1-2*eps.)/(2*n), units = "snpc") # circle radius
    ## generate the coordinates, this fills the area from 1/(2n) + eps to 1 - 1/(2n) - eps
    ## with the appropriate number of centres
    gencoords <- seq(from = offset, to = 1 - offset, by = 1/n)*(1-2*eps.) + eps.
    ## use this to generate x and y coordinates
    ycoords <- rep(rev(gencoords), times = partition) # replicate the y values appropriately
    xcoords <- gencoords[unlist(sapply(partition, seq_len))] # more complicated, generate correct indices
    ## if we want to colour the largest square, identify elements and shade them
    if (coloursq) {
        sqcorner <- max(which(partition >= 1:length(partition))) # find the corner
        cornerinds <- xcoords <= gencoords[sqcorner] & ycoords >= rev(gencoords)[sqcorner] # use this to get the square
        ##sqcorner <- max(which(abs(xcoords + ycoords - 1) < 3*.Machine$double.eps)) # identify the largest value on 1-y=x
        ##cornerinds <- ycoords >= ycoords[sqcorner] & xcoords <= xcoords[sqcorner] # take the corner above this
        params$fill[cornerinds] <- adjustcolor("firebrick", alpha.f = 0.4) # colour the corresponding elements
    }
    ## now call the relevant graphical object function
    if (type == "rect") {
        rectGrob(x = unit(xcoords, "snpc"), y = unit(ycoords, "snpc"), width = width,
                 height = width, gp = params, ...) # must specify both with and height
    } else if (type == "circle") {
        circleGrob(x = unit(xcoords, "snpc"), y = unit(ycoords, "snpc"), r = rad,
                   gp = params, ...) # only radius
    } else stop("Provided 'type' must be one of 'rect' or 'circle'")
}

##' @title Conjugating a Partition Graphical Object
##' @param partgrob a partition graphical object output by part_coords
##' @return the partition conjugate of partgrob
##' @author Chris Salahub and Pavel Schuldiner
grob_conj <- function(partgrob) {
    ## extract the old coordinates
    xold <- as.numeric(partgrob$x)
    yold <- as.numeric(partgrob$y)
    ## reflect these in the line y = 1 - x
    partgrob$x <- unit(1 - yold, units = "snpc")
    partgrob$y <- unit(1 - xold, units = "snpc")
    partgrob
}
