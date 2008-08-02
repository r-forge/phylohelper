\name{contree}
\alias{contree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contree(confile = confile, brlens = F, savepdf = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{confile}{ ~~Describe \code{confile} here~~ }
  \item{brlens}{ ~~Describe \code{brlens} here~~ }
  \item{savepdf}{ ~~Describe \code{savepdf} here~~ }
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
function (confile = confile, brlens = F, savepdf = F) 
{
    library(ape)
    x <- read.nexus(file = confile)
    pptree <- x[[1]]
    plot(pptree)
    if (brlens == T) {
        plot(pptree)
        nodelabels(pptree$node.label, adj = c(1.2, -0.5), frame = "n", 
            cex = 0.8, font = 2)
    }
    else if (brlens == F) {
        plot(pptree, use.edge.length = F)
        nodelabels(pptree$node.label, adj = c(1.2, -0.5), frame = "n", 
            cex = 0.8, font = 2)
    }
    if (savepdf == T) {
        if (brlens == T) {
            pdf(file = "contree\%02d.pdf")
            plot(pptree)
            nodelabels(pptree$node.label, adj = c(1.2, -0.5), 
                frame = "n", cex = 0.8, font = 2)
            dev.off()
        }
        else if (brlens == F) {
            pdf(file = "contree\%02d.pdf")
            plot(pptree, use.edge.length = F)
            nodelabels(pptree$node.label, adj = c(1.2, -0.5), 
                frame = "n", cex = 0.8, font = 2)
            dev.off()
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
