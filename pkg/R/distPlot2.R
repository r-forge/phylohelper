## Plotter of 2 distance matrices (e.g. geographic and genetic distance)
## By Katie Wagner and Marc Cadotte
## Aug 2008

#chars can either be data.frame with species name column and char OR vector with named cells

distPlot2 <- function (char1,char,dist.method="euclidean"){
	
	source("/Users/katiewagner/Documents/Cornell/R Phylo NESCent/distanceplottingfunc/half_circ.R")
	
		
	charD<-as.matrix(char)
	charD<-charD/max(charD)
	char1D<-as.matrix(char1)
	char1D<-char1D/max(char1D)	
			
	maxX<-max(char1D)
	maxY<-max(charD)
	

	char1D[lower.tri(char1D)]<-NA
	charD[lower.tri(charD)]<-NA

	#plot.new()
	par(mfrow=c(1,2))
	#frame()
	plot(0,0,type="n",xlim=c(0,maxX),ylim=c(0,maxY),xlab="Relative Geographic Distance",ylab="Relative Genetic Distance", asp=TRUE)

	df<-data.frame(x=NULL,y=NULL)

	for (i in 1:length(charD[,1])){

		#geo matrix
		centxvec<-char1D[i,]
		#genetic matrix
		centyvec<-charD[i,]

		for (j in i:length(centxvec)){
			x<-as.numeric(centxvec[j])
			y<-as.numeric(centyvec[j])
		
			if (!is.na(x) | !is.na(y))
				vec<-as.vector(c(x,y))
				df<-data.frame(rbind(df,vec))	
		}		
	}	

	df<-df[df$X0!=0,]

	vec1bind<-NULL
	vec2bind<-NULL
	
	for (j in 1:length(centxvec)){
		vec1<-rep(j,each=length(centxvec)-j)
		vec2<-(j+1):length(centxvec)
			
		vec1bind<-c(vec1bind,vec1)
		vec2bind<-c(vec2bind,vec2)
	}
	vec2bind<-vec2bind[1:(length(vec2bind)-2)]	

	coldf<-data.frame(cbind(vec1bind,vec2bind))
	
	final<-data.frame(df,coldf,row.names=c(1:length(coldf$vec1bind)))
	
	colorvec<-hsv(seq(0,0.7,by=.01),1,1,alpha=.7)
	divide<-(floor(length(colorvec)/(length(charD[1,]))))
	choosecols<-seq(1,length(colorvec),by=divide)
	cols<-colorvec[choosecols]

	for (i in 1:length(final$X0)){

		half_circ(centx=final$X0[i],centy=final$X0.1[i],r=((1/600)*max(final)),col1=cols[final$vec1bind[i]],col2=cols[final$vec2bind[i]])
	}


	frame()
	plot.window(xlim=c(1,4),ylim=c(1,length(rownames(charD))))
	points(rep(1,times=length(rownames(charD))),c(length(rownames(charD)):1),col='black',bg=cols[c(unique(final$vec1bind))],pch=21,cex=1.1)
	text(rep(2,times=length(rownames(charD))),c(length(rownames(charD)):1),label=rownames(charD))
	
	return(final)
		
}

gendist<-read.table('/Users/katiewagner/Documents/Cornell/R Course 2007/Pop Gen Analyses/pkazfst1908.txt',header=TRUE,sep='\t',row.names=1)
geodist<-read.table('/Users/katiewagner/Documents/Cornell/R Course 2007/Pop Gen Analyses/sitedist2.txt',header=TRUE,sep='\t',row.names=1)

colnames(gendist)<-c(1:8)
colnames(geodist)<-c(1:8)

gendist<-gendist/(1-gendist)

