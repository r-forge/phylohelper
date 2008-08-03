#plots contree with brlens and pps for clades

contree<-function(confile=confile,brlens=F,savepdf=F){
	library(ape)
	read.nexus(file=confile)->x
	file.name<-attr(x,"origin")
	pptree1<-x[[1]]
	pptree <- ladderize(pptree1, right=FALSE)
	plot(pptree)
	if (brlens==T) {
		plot(pptree)
		nodelabels(pptree$node.label, 
		adj=c(1.2,-0.5),frame="n",cex=0.8,font=2)}
	else if (brlens==F) {
		plot(pptree,use.edge.length=F)
		nodelabels(pptree$node.label, 
		adj=c(1.2,-0.5),frame="n",cex=0.8,font=2)}
	if (savepdf==T) {
		if (brlens==T) {
			pdf(file=paste(file.name,".pdf",sep=""))
			plot(pptree)
			nodelabels(pptree$node.label, 
			adj=c(1.2,-0.5),frame="n",cex=0.8,font=2)
			dev.off()}
		else if (brlens==F) {
			pdf(file=paste(file.name,".pdf",sep=""))
			plot(pptree,use.edge.length=F)
			nodelabels(pptree$node.label, 
			adj=c(1.2,-0.5),frame="n",cex=0.8,font=2)
			dev.off()}
		}
	}

	