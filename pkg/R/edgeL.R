### Edge length accessor to grab by node number
### examples:  edgeL( tree4, 9)
###   edgeL(tree4, c(7,8,9))
###   edgeL(tree4, MRCA(tree4,"Homo", "Pongo"))
###   edgeL(tree4, "Homo")
###   edgeL(tree4, c("Homo", "Galago"))


edgeL <- function(tree, nx) {
	if (is.numeric(nx)) {
		node <- tree@edge[,2]
		return(tree@edge.length[node %in% nx])
	} else {
		nodenames <- c(tree@node.label[-1], tree@tip.label)
		return(tree@edge.length[nodenames %in% nx])
	}
}

## Tests if the given node number or name is the root 

is.root <- function(tree, x) {
	if ( length(ancestor(tree, x))>0 ) return(FALSE)
	else return(TRUE)
	}

## Gets node index(position in edge matrix) based on node name or number. Returns 0 for root

nodenum <- function(tree, node) {
	if (is.root(tree, node)) return (0)
	else return( which(tree@edge[,2] == getnodes(tree, node))) 
	}

### FIX MRCA: self, self should be self, not ancestor.