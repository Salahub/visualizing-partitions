\name{draw_part}
\alias{draw_all_parts}
\alias{draw_part}
\title{Visualizing Partitions}
\description{
This family of functions is used to display partitions graphically. The function \code{draw_part} draws a single partition given a vector of non-negative integers, while the function \code{draw_all_parts} draws all partitions of a non-negative integer \eqn{n}{n} on the same graphical device.
}
\usage{
draw_part(partition, type = "rect", eps = 0.1, params = gpar(), conj = FALSE, ...)
draw_all_parts(n, type = "rect", eps = 0.1, params = gpar(), ...)
}
\arguments{
 \item{partition}{an ordered \code{\link{vector}} of non-negative integers representing a partition}
  \item{n}{a non-negative integer to be partitioned}
  \item{type}{one of "rect" or "circle", the shape of the \code{\link{grob}} elements used to represent the partition}
  \item{eps}{numeric number less than one which specifies the empty buffer distance placed around the partition}
  \item{params}{a \code{\link{list}} of graphical parameters to be passed to the \code{\link{grob}} creation function}
  \item{\dots}{additional arguments passed to the \code{\link{grob}} creation function}
}
\value{
  displays the partition(s) specified
}
\details{
These functions display partitions.
}
\author{
  Chris Salahub and Pavel Schuldiner
}
\references{}
\examples{
## display the partition c(4,1,0,0,0)
draw_part(c(4,1,0,0,0), type = "rect", eps = 0.1)

## draw all partitions of 5
draw_all_parts(5, type = "rect", eps = 0.1)
## change eps
draw_all_parts(5, type = "rect", eps = 0.3)
}