\name{plot_sub_tree}
\alias{plot_sub_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_sub_tree(tree, label, show.edge = T, show.node = T, edge.color = "red", bg.color = "black", node.color = "blue", node.shape = 21)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{ ~~Describe \code{tree} here~~ }
  \item{label}{ ~~Describe \code{label} here~~ }
  \item{show.edge}{ ~~Describe \code{show.edge} here~~ }
  \item{show.node}{ ~~Describe \code{show.node} here~~ }
  \item{edge.color}{ ~~Describe \code{edge.color} here~~ }
  \item{bg.color}{ ~~Describe \code{bg.color} here~~ }
  \item{node.color}{ ~~Describe \code{node.color} here~~ }
  \item{node.shape}{ ~~Describe \code{node.shape} here~~ }
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
function (tree, label, show.edge = T, show.node = T, edge.color = "red", 
    bg.color = "black", node.color = "blue", node.shape = 21) 
{
    if (class(tree) != "phylo" && class(tree) != "phylo4") {
        print("Please use a phylo tree structure")
        return(NA)
    }
    co <- rep(bg.color, length(label$edges))
    co[label$edges] <- edge.color
    if (show.edge) {
        plot(tree, edge.col = co)
    }
    else {
        plot(tree)
    }
    if (show.node) {
        nodelabels(pch = node.shape, bg = node.color, node = which(label$nodes))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
