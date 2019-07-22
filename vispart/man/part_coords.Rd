\name{part_coords}
\alias{part_coords}
\alias{grob}
\title{Generating a Graphical Partition Object}
\description{
  This function uses the \code{grid} package to generate a \code{grid} \code{\link{grob}} which corresponds to a given partition when drawn.
}
\usage{
part_coords(partition, type = "rect", eps = 0.1, coloursq = FALSE, params = gpar(), ...)
}
\arguments{
  \item{partition}{an ordered \code{\link{vector}} of non-negative integers representing a partition}
  \item{type}{one of "rect" or "circle", the shape of the \code{\link{grob}} elements used to represent the partition}
  \item{eps}{numeric number less than one which specifies the empty buffer distance placed around the partition}
  \item{coloursq}{logical value indicating whether the largest square of the partition should be shaded in red}
  \item{params}{a named \code{\link{list}} of graphical parameters to be passed to the \code{\link{grob}} creation function}
  \item{\dots}{additional arguments passed to the \code{\link{grob}} creation function}
}
\value{
  a \code{grid} \code{\link{grob}} (graphical object)
}
\details{
  This function plots a partition, specified as a vector of non-negative integers, using either squares or circles in a Young tableau. Note that the use of the \code{coloursq} option will override any specified shading provided by the \code{params} argument, which is a named list of graphical parameters which follows the \code{grid} format.
}
\references{
  Yong, Alexander. (2007). \emph{What is a young tableau}. Notices of the AMS 54.2: 240-241.
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

## colour the major square of prt
prt <- part_coords(c(4,1,0,0,0), coloursq = TRUE)
grid.draw(prt)
}