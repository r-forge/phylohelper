## Plotter of 2 distance matrices (phylogeny and character)
## By Katie Wagner and Marc Cadotte
## Aug 2008

distPlot <- function 
(tree,char, dist.method="manhattan",scheme="color",circles="small",comparison="tree-char") {
	
	require(ape)
if (comparison=="tree-char") {

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
		}

if (comparison == "char-char") {
	treeD<-tree
	 charD<-char
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

	par(mfrow=c(1,2))
	maxX<-max(final$X0)-min(final$X0)

	if (circles=="small"){
		r=((1/40)*maxX)
	}
	
	if (circles=="large"){
	r=((1/25)*maxX)
	}

	colorvec<-hsv(seq(0,0.8,by=0.01),1,1,alpha=0.7)
	div<-(floor(length(colorvec)/length(unique(final$vec1bind))))
	choosecols<-seq(1,length(colorvec),by=div)
	cols<-colorvec[choosecols]
	
if (comparison=="tree-char") {
	
	
	plot(0,0,type="n",xlim=c(min(final$X0),1),
	ylim=c(min(final$X0.1),1),
	xlab="Relative phylogenetic distance",
	ylab="Relative character distance",asp=T)

if (scheme=="color") {

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

	cols<-c(colors()[262:361],"black")

	for (i in 1:length(final$X0)){

		half_circ(centx=final$X0[i],
		centy=final$X0.1[i],
		r,
		col1=cols[final$vec1bind[i]],
		col2=cols[final$vec2bind[i]])
	}
	plot(tree,tip.color=cols)
	}
	}
	
	if (comparison=="char-char") {
	
	plot(0,0,type="n",xlim=c(min(final$X0),1),
	ylim=c(min(final$X0.1),1),
	xlab="Relative character distance 1",
	ylab="Relative character distance 2",asp=T)
	
	
if (scheme=="color") {

	for (i in 1:length(final$X0)){

		half_circ(centx=final$X0[i],
		centy=final$X0.1[i],
		r,
		col1=cols[final$vec1bind[i]],
		col2=cols[final$vec2bind[i]])
	}
	frame()
	plot.window(xlim=c(1,4),ylim=c(1,length(unique(final$vec1bind))))
	points(rep(1,times=length(unique(final$vec1bind))),
		c(length(unique(final$vec1bind))):1,
		col="black",
		bg=cols[length(unique(final$vec1bind)):1],
		pch=21,
		cex=1.1	)
	text(rep(2,times=length(unique(final$vec1bind))),
		c(length(unique(final$vec1bind))):1,
		label=rownames(charD))
	}

if (scheme=="grey") {

	cols<-c(colors()[262:361],"black")

	for (i in 1:length(final$X0)){

		half_circ(centx=final$X0[i],
		centy=final$X0.1[i],
		r,
		col1=cols[final$vec1bind[i]],
		col2=cols[final$vec2bind[i]])
	}
	}
	}

	
  	
}


