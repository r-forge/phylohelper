\name{distPlot}
\alias{distPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
distPlot(tree,char,dist.method="manhattan",scheme="color",circles="small")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{ A phylogeny object of class "phylo" }
  \item{char}{ A 2-column dataframe with taxa names in the first column and character values in the second. Taxa must have same names as tip labels in the phylogeny, but do not need to be in the same order. }
  \item{dist.method}{ Select distance method for constructing character distance matrix. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". }
  \item{scheme}{ Set "color" or "grey" for graphic output. }
  \item{circles}{ Set point size as "small" or "large" }
  \item{comparison}{ Default is for a phylogeny and character dataframe ("tree-char"), but user can input two character distance matrices by selecting "char-char". }
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
\author{ Catherine E Wagner & Marc W Cadotte }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##plotting phylogenetic and character distances in color
char<-rnorm(10,10,10)
tree<-chronogram(rtree(10))
char<-data.frame(rname=(tree$tip.label[sample(1:10)]),char)
distPlot(tree,char)

## plot two character distance matrices in grey scale
char1<-as.matrix(dist(rnorm(12,10)))
char2<-as.matrix(dist(rnorm(12,10)))
distPlot(char1,char2,comparison="char-char",scheme="grey")




## The function is currently defined as
distPlot <- function (tree,char,dist.method="manhattan",scheme="color",circles="small"){
	


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
