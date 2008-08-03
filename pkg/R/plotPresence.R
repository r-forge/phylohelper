### plotPresence.R
### written by François Michonneau & Mark Cornwall
### Aug 1st 2008, during NESCent Phyloinformatics Summer course


## TODO - try to see if node labels are preserved if the initial tree has some
## TODO - provide arguments for plotting
## TODO - write documentation

plotPresence <- function(tree, presenceData, plotResults = TRUE,
                         returnData = TRUE, colEdges = c("red", "blue"), ...) {
    ## ... for further plotting arguments
    stopifnot(require(phylobase)) ## can be removed once we document dependencies

    if(!all(unlist(presenceData) %in% c(0,1)))
        stop("\'presenceData\' should only contains 0 and 1")

    if(class(tree) == "phylo4d") {
        warning("Your associated data are being erased.")
    }
    if(class(tree) != "phylo4")
        tree <- as(tree, "phylo4")

    ## probably unecessary
    if(nrow(presenceData) != nTips(tree)) {
        stop("Your presence data don't have the same length than your number of tips")
    }

    nodeData <- matrix(rep(0, nNodes(tree)*ncol(presenceData)), nrow=nNodes(tree))

    if(!is.data.frame(presenceData)) {
        presenceData <- data.frame(presenceData)
    }

    if(!any(rownames(presenceData) %in% labels(tree))) {
        stop("Your tip labels needs to be the row names of \'presenceData\'.
                 Sorry, but I don't want to label automatically your data.")
    }

    nodeData <- data.frame(nodeData, row.names = nodeLabels(tree))

    ## get row names (tip labels) for species present
    nmSubTr <- apply(presenceData, 2, function(x) {
        rownames(presenceData)[x == 1]
    })

    lstAnc <- lapply(nmSubTr, function(eachLocation) {
        lapply(eachLocation,
               function(x) {
                   names(ancestors(tree, x))   # return node names
               })
    })

    lstAnc <- lapply(lstAnc, function(eachLocation) {
        unique(unlist(eachLocation))        # make list of node names unique
    })

    for(i in 1:ncol(nodeData)) {
        indLstAnc <- lstAnc[[i]]
        nodeData[indLstAnc, ][i] <- 1        # replace ancestral nodes where true with 1
    }

    ## reconstruct the tree
    tree <- phylo4d(tree, tip.data = locData, node.data = nodeData,
                    use.tip.names = T, use.node.names = T, label.type ="row.names")
    treePhy <- as(tree, "phylo")
    treeData <- as(tree, "data.frame")
    treeEd <- edges(tree)

   print(tree)
    print(treeData)



    ## assign colors for presence absence data, for each edge
    nmLoc <- names(presenceData)
    lEdges <- lapply(nmLoc, function(eachLoc) {
        PrAbsLoc <- apply(treeEd, 1, function(x) {
            treeData[treeData$ancestor == x[1] &
                     treeData$node == x[2],][eachLoc]
        })
        PrAbsLoc <- unlist(PrAbsLoc)
    })

    if(plotResults) {
        if(ncol(presenceData) > 5) {
            stillPlot <- readline("Warning: You are about to plot more than 5 windows.\n
                                   If you prefer, you can give me a subset of your presence-
                                   absence data. Do you want to proceed? (yes/no) \n \n")
            stillPlot <- match.arg(stillPlot)
        }
        else stillPlot <- "yes"

        if(stillPlot == "yes") {
            plotEdges <- lapply(lEdges, function(eachLoc) {
                vectorColEdge <- unlist(lapply(eachLoc == 1,
                                         function(x)
                                         if(x) colEdges[1]
                                         else colEdges[2]))
                eval(call(options()$device))
                plot(treePhy, edge.color = vectorColEdge, ...)
            })
        }
    }

    if(returnData) return(tree)
}


### Example
## rTr <- rcoal(30)
## rTr <- as(rTr, "phylo4")
## locData <- matrix(sample(c(0,1), size=90, replace=T), nrow=30)
## locData <- data.frame(locData, row.names = labels(rTr))
## plotPresence(rTr, locData)
