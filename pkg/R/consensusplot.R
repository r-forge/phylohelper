branch.height <- function (node, anc, bl, k) {
  ## this recursion might prove expensive for large trees
  ## there should be a direct method 
  if (is.na(anc[k])) {
    0
  } else {
    bl[k]+Recall(node,anc,bl,anc[k])
  }
}

count.nodes <- function(node, anc, bln, k) {

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
  tdat <- as(tree4, "data.frame")[-1,]
tdat$ape_order <- 1:nrow(tdat)
tdat <- tdat[order(tdat$node),]
#  oo <- order(tdat$node)

  ape_order <- 1:length(tdat$node)
  oo <- order(tdat$node)
  tdat <- tdat[oo,]
  
  node <- tdat$node
  label <- tdat$label
  ancestor <- tdat$ancestor
  branch.length <- tdat$branch.length
  node.type <- tdat$node.type
 
  lineages <- lapply(node, function(x) ancestors(tree4, x))
  stepstoroot <- sapply(lineages, length)
  
## assign heights to terminal taxa, max tree height. root = 0
  height <- rep(NA, length(node))
  height[node.type=="tip"] <- maxsteps <- max(stepstoroot)
  
## split nodes into groups by steps to root
  nodebysteps <- split(node, stepstoroot)
  
## Pair up nodes to form "cherries" (nodes grouped by ancestor), still grouped by steps to root
## then reverse order by steps
  cherries_steps <- lapply(nodebysteps, 
                      function(x) {
                      	split(x, ancestor[x])
                      }
                    )[ as.character(maxsteps:1) ]
                    
## Calculate the height of each ancestor, need loop because calculations are iterative                   
  for (cherries in cherries_steps) {
  	 hh <- sapply( cherries,
  	                 function(cherry)
  	                 {
  	                 	  height[unique]})}                    
                    
                    
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
	
