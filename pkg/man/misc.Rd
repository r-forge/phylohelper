\name{%w/o%}
\alias{%w/o%}
\title{ Miscellaneous functions}
\description{
A collection of useful functions
}
\usage{
%w/o%(x, y)
}
\arguments{
  \item{x}{ a string object }
  \item{y}{ another string object }
}
\details{
%w/o% is a string matching function: x without y
}
\value{
A vector of characters
}
\author{ copied from R help files}
\examples{
A <- c("a", "b", "c")
B <- c("a", "c", "f")

A %w/o% B

## The function is currently defined as
function (x, y) 
x[!x \%in\% y]
}
\keyword{ string matching }
\keyword{ string }
\keyword{ string function }