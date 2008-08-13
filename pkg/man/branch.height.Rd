\name{branch.height}
\alias{branch.height}
\alias{assign.lengths}

\title{Branch length functions}
\description{
Computes the height (sum of branch lengths from root to the given node) or the branch length of the given node.
}
\usage{
branch.height(node, anc, bl, k)
assign.lengths(node, anc, newbl, k)
count.nodes(node, anc, bln, k)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{a vector containing node numbers }
  \item{anc}{ a vector containing ancestor numbers }
  \item{bl}{ vector of branch lengths }
  \item{newbl}{ vector of branch lengths }
  \item{bln}{ vector of branch lengths as number of nodes from root }
  \item{k}{ the node to go backwards on }
}
\value{
 a numerical value, the branch height or vector of branch lengths
}
\author{ Marguerite Butler and Aaron King }
\note{ This uses a recursion. There may be a faster algorithm. 

}
\examples{


## The function is currently defined as
function (node, anc, bl, k) 
{
    if (is.na(anc[k])) {
        0
    }
    else {
        bl[k] + Recall(node, anc, bl, anc[k])
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ methods }