\name{count.bl}
\alias{count.bl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
count.bl(tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{ ~~Describe \code{tree} here~~ }
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
function (tree) 
{
    if (!inherits(tree, "phylo")) 
        stop(sQuote("tree"), " must be of class ", sQuote("phylo"))
    tree4 <- as(tree, "phylo4")
    tree4.dat <- as(tree4, "data.frame")
    oo <- order(node)
    tree4.dat <- tree4.dat[oo, ]
    attach(tree4.dat)
    tips <- which(node.type == "tip")
    nnodes <- sapply(node, function(k) count.nodes(node, ancestor, 
        0, k))
    times <- rep(NA, length(node))
    bls <- assign.lengths(node, ancestor, times, 1)
    max.height <- max(nnodes)
    tree.depth <- max(nnodes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
