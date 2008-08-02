\name{label_sub_node}
\alias{label_sub_node}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
label_sub_node(tree, tips)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{ ~~Describe \code{tree} here~~ }
  \item{tips}{ ~~Describe \code{tips} here~~ }
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
function (tree, tips) 
{
    if (class(tree) != "phylo" && class(tree) != "phylo4") {
        print("Please use a phylo tree structure")
        return(NA)
    }
    del <- tree$tip.label \%in\% tips
    if (sum(del) < 2) {
        print("Please select 2 or more tips\n")
        return(NA)
    }
    node_tmp = rep(0, tree$Nnode + length(tree$tip.label))
    node_ret = rep(0, (tree$Nnode) + length(tree$tip.label))
    for (i in 1:length(tree$tip.label)) {
        node_ret[i] <- node_tmp[i] <- del[i]
    }
    for (num in seq(length(node_tmp), length(tree$tip.label), 
        -1)) {
        childrn <- node_tmp[tree$edge[tree$edge[, 1] == num, 
            2]]
        node_ret[num] <- childrn[1] & childrn[2]
        node_tmp[num] <- childrn[1] | childrn[2]
    }
    edge <- rep(0, length(tree$edge)/2)
    root <- 0
    i <- length(tree$tip.label)
    while (root == 0) {
        i <- i + 1
        if (node_ret[i] == 1) {
            root = c(i)
        }
    }
    strt <- which(del)
    for (i in 1:length(strt)) {
        tmp_node <- strt[i]
        edge[tree$edge[, 2] == tmp_node] = 1
        tmp_node <- c(tree$edge[tree$edge[, 2] == tmp_node, 1])
        while (tmp_node > root) {
            edge[tree$edge[, 2] == tmp_node] = 1
            tmp_node <- c(tree$edge[tree$edge[, 2] == tmp_node, 
                1])
        }
    }
    return(list(nodes = as.logical(node_ret), edges = as.logical(edge)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
