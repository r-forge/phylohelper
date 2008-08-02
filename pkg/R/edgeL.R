### Edge length accessor to grab by node number
### examples:  edgeL( tree4, 9)
###   edgeL(tree4, c(7,8,9))
###   MRCA(tree4,"Homo", "Pongo")

edgeL <- function(tree, nx) {
	node <- tree@edge[,2]
	return(tree@edge.length[node %in% nx])
	}