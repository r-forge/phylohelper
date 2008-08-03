branch.height <- function (node, anc, bl, k) {
  ## this recursion might prove expensive for large trees
  ## there should be a direct method 
  if (is.na(anc[k])) {
    0
  } else {
    bl[k]+Recall(node,anc,bl,anc[k])
  }
}

countnodes <- function(node, anc, bln, k) {

	if (is.na(anc[k]) | is.numeric(bln[k])) {
		0
	} else {
		1 + Recall(node, anc, bln, anc[k])
	}
}


assign.lengths <- function(node, anc, height, newbl, k) 
{
    while (!is.na(anc[k])) 
    {
    	if (is.na(newbl[k])) 
    	{  
    		newbl[k] <- height[k] - height[anc[k]]
    		if (is.na(newbl[k]))  newbl[k] <- 1 
       }
    	k <- anc[k]
    } 
    newbl[k] = 0
    return(newbl)
} 

#tree <- read.tree(text="((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49): 0.13,Ateles:0.62):0.38,Galago:1.00);")

count.bl <- function (tree)  {
  if (!inherits(tree, 'phylo'))
	stop(sQuote("tree"), " must be of class ",sQuote("phylo"))

  tree4 <- as(tree, "phylo4") 
  tree4.dat <- as(tree4, "data.frame") 
  oo <- order(tree4.dat$node)
  tree4.dat <- tree4.dat[oo,]

  attach(tree4.dat)  # label node ancestor branch.length node.type
  
  stepstoroot <- sapply(node, function(k) count.nodes(node, ancestor, rep(NA, length(node)), k))

  ti <- which(node.type=="tip") 
  tips <- node[ti]
  tsteps <- stepstoroot[ti]

  bln <- rep(NA, length(node))

  treeheight <- maxsteps <- max(stepstoroot)
  
  cherries <- which(stepstoroot==maxsteps)  # find cherries
  
  pairs <- split(cherries, ancestor[cherries])  # identify pairs
  for (i in pairs) { bln[i] <- 1 }   # assign terminal cherries bl=1
  pairancs <- as.numeric(names(pairs)) # find ancestors to each pair
  					# update branch lengths from each pair back to root 
  					# (or until hit other branch lengths)

  height <- stepstoroot*bln  # assign height of nodes to nodes along paths 
                             # to tips with maxsteps
  height[tips] <- treeheight

  bln <- lapply(pairancs, function(x) assign.lengths(node, ancestor, height, bln, x))[[1]]
  anclist <- lapply(pairancs, function(x) ancestors(tree,x))
  ancl <- c(pairancs[1], unlist(anclist[[1]]))  
  blxx <- unlist(lapply(ancl, function(x) branch.height(node, ancestor, bln, x)))

  height[ancl] <- blxx
     
}
	
