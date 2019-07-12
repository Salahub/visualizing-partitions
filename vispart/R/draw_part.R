### The Draw Part Function ###############################################################

##' @title Drawing a Single Partition
##' @param partition a partition represented as a sequence of positive integers
##' @param type one of "circle" or "rectangle"
##' @param eps a positive real number
##' @return a graphical object and a plot of the partition
##' @author Chris Salahub
draw_partition <- function(partition, type = c("circle","rectangle"),
                           eps = 0.1) {
    n <- sum(partition) # the value being partitioned, needed for checks and calculations
    ## perform checks
    stopifnot(all(partition >= 0)) # check the partition provided is valid
    if (length(partition) < n) partition <- c(partition, rep(0, n - length(partition))) ## nice input handling
    ## calculate useful constants
    eps. <- eps*1/n # scale the eps value appropriately
    offset <- 1/(2*n) # the offset to place the centres correctly
    width <- (1-2*eps.)/n # the square width
    ## generate the coordinates, this fills the area from 1/(2n) + eps to 1 - 1/(2n) - eps
    ## with the appropriate number of centres
    gencoords <- seq(from = offset, to = 1 - offset, by = 1/n)*(1-2*eps.) + eps.
    ## use this to generate x and y coordinates
    ycoords <- rep(rev(gencoords), times = partition) # simply replicate appropriately
    xcoords <- gencoords[unlist(sapply(partition, seq_len))] # more complicated, generate correct sequences for indexing
    ## plot using the x and y coordinates
    grid.newpage()
    grid.rect(x = xcoords, y = ycoords, width = width, height = width)
}


