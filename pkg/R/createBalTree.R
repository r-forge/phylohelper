### creates a balanced tree
createBalTree <- function(Ntips = 8) {

    ## checks number of tips
    logNtips <- log(Ntips, b=2)
    if(logNtips != as.integer(logNtips)) stop("invalid number of tips")

    ## creates tree labels
    originalLabels <- as.character(1:Ntips)

    tLabels <- matrix(originalLabels, ncol=2, byrow=T)

    formatLabels <- function(x) {
        toRet <- paste("(", x[1], ",", x[2], ")", sep = "")
        toRet
    }

    tLabels <- matrix(apply(tLabels, 1, formatLabels), ncol=2, byrow=T)


    while(length(tLabels) != 2) {
        tLabels <- matrix(tLabels, ncol=2, byrow=T)
        tLabels <- apply(tLabels, 1, formatLabels)
    }
    if(length(tLabels) == 2)
        tLabels <- formatLabels(tLabels)

    tLabels <- paste(tLabels, ";",sep = "")

    if(require("ape")) { ## needs to be removed if ape loaded by default
        tree <- read.tree(text = tLabels,)
        tree$tip.label <- sort(as.numeric(originalLabels), decreasing = T)
        if(is.numeric(tree$tip.label))
            tree$tip.label <- as.character(tree$tip.label)
        tree$edge.length <- rep(1, length(tree$edge))
        return(tree)
    }
}
