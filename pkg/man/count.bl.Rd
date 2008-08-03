\name{count.bl}
\alias{count.bl}

\title{ Function to convert branch lengths to cladogram plotting branch lengths }
\description{
This function currently does not work. The idea is to compute the branch lengths in order to plot a true "cladogram" (just topology, uninformative of branch lengths).
}
\usage{
count.bl(tree)
}
\arguments{
  \item{tree}{ a phylogenetic tree able to be coerced to class phylo4 }
}
\value{
  a numeric vector of branch lengths
}
\author{ Marguerite Butler }
\note{ Does not work
}
\examples{

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
\keyword{ phylogeny }
\keyword{ cladogram }