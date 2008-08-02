\name{assign.lengths}
\alias{assign.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
assign.lengths(node, anc, newbl, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{ ~~Describe \code{node} here~~ }
  \item{anc}{ ~~Describe \code{anc} here~~ }
  \item{newbl}{ ~~Describe \code{newbl} here~~ }
  \item{k}{ ~~Describe \code{k} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (node, anc, newbl, k) 
{
    while (!is.na(anc[k])) {
        if (is.na(newbl[k])) {
            newbl[k] <- 1
        }
        k <- anc[k]
    }
    newbl[k] = 0
    return(newbl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
