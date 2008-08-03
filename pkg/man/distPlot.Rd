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
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
char<-rnorm(10,10,10)
tree<-chronogram(rtree(10))
char<-data.frame(rname=(tree$tip.label[sample(1:10)]),char)
distPlot(tree,char)



## The function is currently defined as
distPlot <- function (tree,char,dist.method="manhattan",scheme="color",circles="small"){
	
	require(ape)

	if (is.ultrametric(tree)==F) {
		tree<-chronogram(tree)
		}
		
		
	if (class(char)=="data.frame") {
		
		names(char)<-c("rname","char")
		s.ord<-match(tree$tip.label,char$rname)
		char<-char[s.ord,]
		
		rownames(char)<-char$rname
		
		charD<-as.matrix(dist(char[2],method=dist.method))
		treeD<-as.matrix(as.dist(2*max(tree$edge.length)-2*(vcv.phylo(tree))))
		
		}
	if (class(char)!="data.frame")	 {
			print("Need 2 column dataframe!")
			char=0
			}	

treeD[lower.tri(treeD)]<-NA
charD[lower.tri(charD)]<-NA

df<-data.frame()

for (i in 1:length(rownames(treeD))){

	#phylo matrix
	centxvec<-treeD[i,]
	#character matrix
	centyvec<-charD[i,]

	for (j in i:length(centxvec)){
		x<-centxvec[j]
		y<-centyvec[j]
		
		if (!is.na(x) | !is.na(y))
			vec<-c(x,y)
			df<-data.frame(rbind(df,vec))	
	}		
}	

df<-df[df$X0!=0 & df$X0.1!=0,]

vec1bind<-NULL
vec2bind<-NULL
	
for (j in 1:length(centxvec)){
	vec1<-rep(j,times=length(centxvec)-j)
	
	vec1bind<-c(vec1bind,vec1)
	
	if (j<length(centxvec)) {
	vec2<-(j+1):length(centxvec)
	vec2bind<-c(vec2bind,vec2)
	}	
}

	coldf<-data.frame(cbind(vec1bind,vec2bind))
	df<-data.frame(df[1]/max(df[1]),df[2]/max(df[2]))
	final<-data.frame(df,coldf,
	row.names=c(1:length(coldf$vec1bind)))
	
	half_circ<-function(centx,centy,r,col1,col2){
        theta<-seq(-pi,pi,length=100)
        polygon(centx+r*sin(theta),centy+r*cos(theta), col=col1)
        theta<-seq(-pi,0,length=50)
        polygon(centx+r*sin(theta),centy+r*cos(theta), col=col2)
	}

	
	#plot.new()
	par(mfrow=c(1,2))
	#frame()
	plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="Relative phylogenetic distance",
	ylab="Relative character distance",asp=T)
	
	if (circles=="small"){
		r=((1/40)*maxX)
	}
	
	if (circles=="large"){
	r=((1/20)*maxX)
	}

if (scheme=="color") {

	cols<-rainbow(length(centyvec))

	for (i in 1:length(final$X0)){

		half_circ(centx=final$X0[i],
		centy=final$X0.1[i],
		r,
		col1=cols[final$vec1bind[i]],
		col2=cols[final$vec2bind[i]])
	}
	plot(tree,tip.color=cols)
	}

if (scheme=="grey") {

	cols<-c("black",colors()[262:361])

	for (i in 1:length(final$X0)){

		half_circ(centx=final$X0[i],
		centy=final$X0.1[i],
		r,
		col1=cols[final$vec1bind[i]],
		col2=cols[final$vec2bind[i]])
	}
	plot(tree,tip.color=cols)
	}
	

	return(final)
	
  	
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
