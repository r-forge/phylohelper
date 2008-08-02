branch.height <- function (node, anc, bl, k) {
  ## this recursion might prove expensive for large trees
  ## there should be a direct method 
  if (is.na(anc[k])) {
    0
  } else {
    bl[k]+Recall(node,anc,bl,anc[k])
  }
}

count.nodes <- function(node, anc, nn, k) {
	if (is.na(anc[k])) {
		0
	} else {
		1 + Recall(node, anc, nn, anc[k])}
	}



assign.lengths <- function(node, anc, newbl, k) 
{
    while (!is.na(anc[k])) 
    {
    	if (is.na(newbl[k])) 
    	{
    		newbl[k] <- 1
       }
    	k <- anc[k]
    } 
    newbl[k] = 0
    return(newbl)
} 

count.bl <- function (tree)  {
  if (!inherits(tree, 'phylo'))
	stop(sQuote("tree"), " must be of class ",sQuote("phylo"))

  tree4 <- as(tree, "phylo4") 
  tree4.dat <- as(tree4, "data.frame") 
  oo <- order(node)
  tree4.dat <- tree4.dat[oo,]
  
  attach(tree4.dat)  # label node ancestor branch.length node.type
  
  tips <- which(node.type=="tip")
  nnodes <- sapply(node, function(k) count.nodes(node, ancestor, 0, k))
  
  times <- rep(NA, length(node))
  bls <- assign.lengths(node, ancestor, times, 1)
  max.height <- max(nnodes)


  tree.depth <- max(nnodes)
  
  }

	
