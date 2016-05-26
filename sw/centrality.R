
source("loadSW.R")
source("plotSW.R")

# Plot the scenes graph for reference.
plot.sw.graph(scenes,"Scenes")

# Get the adjacency matrix. (as.matrix() to make non-sparse.)
A <- as.matrix(get.adjacency(scenes))

# The Perron-Frobenius eigenvector is the eigenvector that goes with the
# highest eigenvalue of the adjacency matrix. (Take the abs() because this
# vector may be all-negative instead of all-positive.)
pf.v <- abs(eigen(A)$vectors[,1])

most.central.chars <- 
    data.frame(char=V(scenes)[order(pf.v,decreasing=TRUE)]$name,
               pf.val=round(sort(pf.v,decreasing=TRUE),3))
print(most.central.chars)
