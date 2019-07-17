### Generating Partition Graphical Objects ###############################################

##' @title Generating Partition Graphical Objects
##' @param partition a partition represented as a sequence of positive integers
##' @param type one of "circle" or "rectangle"
##' @param eps a positive real number
##' @param params a list of graphical parameters to be applied to the output
##' @return a graphical object corresponding to a plot of the partition
##' @author Chris Salahub and Pavel Schuldiner
part_coords <- function(partition, type = "rect",
                        eps = 0.1, params = gpar(), ...) {
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
    ycoords <- unit(rep(rev(gencoords), times = partition), "snpc") # simply replicate appropriately
    xcoords <- unit(gencoords[unlist(sapply(partition, seq_len))], "snpc") # more complicated, generate correct sequences for indexing
    ## now call the relevant graphical object function
    if (type == "rect") {
        rectGrob(x = xcoords, y = ycoords, width = width, height = width,
                 gp = params, ...) # must specify both with and height
    } else if (type == "circle") {
        circleGrob(x = xcoords, y = ycoords, r = width/2, gp = params, ...) # only radius
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

