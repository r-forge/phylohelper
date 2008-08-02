#plots contree with brlens and pps for clades
#file="carnivore_molecular.nex.con"

contree<-function(confile=confile,brlens=F,savepdf=F){
	library(ape)
	read.nexus(file=confile)->x
	pptree<-x[[1]]
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
			pdf(file="contree%02d.pdf")
			plot(pptree)
			nodelabels(pptree$node.label, 
			adj=c(1.2,-0.5),frame="n",cex=0.8,font=2)
			dev.off()}
		else if (brlens==F) {
			pdf(file="contree%02d.pdf")
			plot(pptree,use.edge.length=F)
			nodelabels(pptree$node.label, 
			adj=c(1.2,-0.5),frame="n",cex=0.8,font=2)
			dev.off()}
		}
	}

	
	#if brlens=F, will give ultrametric tree with grafen branch lengths
	#tell folks what name pdf gets and where it goes -- same folder
