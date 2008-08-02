#given a tree and a list of tips within the tree, returns a list with 
label_sub_node <- function(tree, tips)
{
	#This also works with a phylo or a phylo4 class- the ape type of tree
	if (class(tree) != "phylo" && class(tree) != "phylo4")
	{
		print("Please use a phylo tree structure");
		return(NA);
	}
	#Finding the tips with the 
	del <- tree$tip.label %in% tips;
	if (sum(del) < 2)
	{
		print("Please select 2 or more tips\n");
		return(NA);
	}
	#Making the temporary and the node
	node_tmp = rep(0, tree$Nnode + length(tree$tip.label));
	node_ret = rep(0, (tree$Nnode)+ length(tree$tip.label));
	for (i in 1:length(tree$tip.label))
	{
		node_ret[i] <- node_tmp[i] <- del[i];
	
	}
	#Goes is reverse order through the interior nodes and determines both whether it is included
	#(both children are have an included descendent) and whether at least an included decendents
	for (num in seq(length(node_tmp), length(tree$tip.label), -1))
	{
		childrn <- node_tmp[tree$edge[tree$edge[,1] == num,2]];
		node_ret[num] <- childrn[1] & childrn[2];
		node_tmp[num] <- childrn[1] | childrn[2];
	}
	#Initializing the edge vector
	edge <- rep(0, length(tree$edge)/2);
	#Finding the root: the first interior node labeled as true
	root <- 0;
	i <- length(tree$tip.label);
	while(root == 0)
	{
		i <- i +1;
		if (node_ret[i] == 1)
		{
			root = c(i)
		}
	}
	#Labeling all the edges higher than the root connected 
	#Getting all the roots for which there is a start;
	strt <- which(del);
	for (i in 1:length(strt))
	{
		tmp_node <- strt[i];
		edge[tree$edge[,2]==tmp_node] =1;
		tmp_node <- c(tree$edge[tree$edge[,2]==tmp_node, 1]);
		while(tmp_node > root)
		{
			edge[tree$edge[,2]==tmp_node] =1;
			tmp_node <- c(tree$edge[tree$edge[,2]==tmp_node, 1]);
		}
	}
	return(list(nodes=as.logical(node_ret),edges=as.logical(edge)));
}

plot_sub_tree <- function(tree, label, show.edge=T, show.node=T, edge.color="red", bg.color="black", node.color="blue", node.shape=21)
{
	if (class(tree) != "phylo" && class(tree) != "phylo4")
	{
		print("Please use a phylo tree structure");
		return(NA);
	}
	co <- rep(bg.color, length(label$edges));
	co[label$edges] <- edge.color;
	if (show.edge)
	{
		plot(tree, edge.col=co);
	}
	else
	{
		plot(tree);
	}
	if (show.node)
	{
		nodelabels(pch=node.shape, bg=node.color, node=which(label$nodes));
	}
}
	
