\name{grob_conj}
\alias{grob_conj}
\title{Conjugating a Graphical Partition Object}
\description{
  This function conjugates a partition graphical object by reflecting its coordinates in the line \eqn{y = 1-x}{y = 1-x}.
}
\usage{
grob_conj(partgrob)
}
\arguments{
  \item{partgrob}{a graphical partition object as output by \code{\link{part_coords}}}
}
\value{
  a \code{grid} \code{\link{grob}} (graphical object)
}
\details{
  This function creates the conjugate partition graphical object of the provided partition graphical object by taking \eqn{x = 1 -y}{x_{new} = 1-y} and \eqn{y = 1 - x}{y_{new} = 1 - x}.
}
\author{
  Chris Salahub and Pavel Schuldiner
}
\examples{
library(grid)
## generate the object for the partition c(4,1,0,0,0)
prt <- part_coords(c(4,1,0,0,0), type = "rect", eps = 0.1)
grid.draw(prt)
## conjugate it
grid.newpage()
grid.draw(grob_conj(prt))
}