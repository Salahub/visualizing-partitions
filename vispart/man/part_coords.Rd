\name{part_coords}
\alias{part_coords}
\alias{grob}
\title{Generating a Graphical Partition Object}
\description{
  This function uses the \code{grid} package to generate a \code{grid} \code{\link{grob}} which corresponds to a given partition when drawn.
}
\usage{
part_coords(partition, type = "rect", eps = 0.1, params = gpar(),
...)
}
\arguments{
  \item{partition}{an ordered \code{\link{vector}} of non-negative integers representing a partition}
  \item{type}{one of "rect" or "circle", the shape of the \code{\link{grob}} elements used to represent the partition}
  \item{eps}{numeric number less than one which specifies the empty buffer distance placed around the partition}
  \item{params}{a \code{\link{list}} of graphical parameters to be passed to the \code{\link{grob}} creation function}
  \item{\dots}{additional arguments passed to the \code{\link{grob}} creation function}
}
\value{
  a \code{grid} \code{\link{grob}} (graphical object)
}
\details{
  This function uses the provided vector of non-negative integers to plot either rectangles or circles which represent the partition.
}
\author{
  Chris Salahub and Pavel Schuldiner
}
\examples{
library(grid)
## generate the object for the partition c(4,1,0,0,0)
prt <- part_coords(c(4,1,0,0,0), type = "rect", eps = 0.1)
grid.draw(prt)

## increase eps and change the type to circles
prt2 <- part_coords(c(4,1,0,0,0), type = "circle", eps = 0.5)
grid.draw(prt2)
}