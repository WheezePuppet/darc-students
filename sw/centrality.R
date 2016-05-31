
library(igraph)

source("loadSW.R")
source("plotSW.R")

# Plot the scenes graph for reference.
plot.sw.graph(scenes,"Scenes")

# Get the adjacency matrix. (as.matrix() to make non-sparse.)
A <- as.matrix(get.adjacency(scenes))


# I. Perron-Frobenius centrality.

# The Perron-Frobenius eigenvector is the eigenvector that goes with the
# highest eigenvalue of the adjacency matrix. (Take the abs() because this
# vector may be all-negative instead of all-positive.)
pf.v <- abs(eigen(A)$vectors[,1])

most.central.chars <- round(sort(pf.v,decreasing=TRUE),3)
names(most.central.chars) <- V(scenes)[order(pf.v,decreasing=TRUE)]$name

cat("\nStephen says:\n")
print(most.central.chars)

cat("\nevcent says:\n")
print(round(sort(evcent(scenes)$vector,decreasing=TRUE),3))


cat("\n\n")


# II. Katz centrality.

# Compute the maximum "legal" value of alpha.
# (Note: if directed graph, this eigenval might be complex, no?)
max.alpha <- 1/eigen(A)$values[1]

# For no particularly good reason, choose half that value as our alpha.
alpha <- max.alpha / 2

# Compute the "centrality matrix" Q, for this alpha.
Q <- solve(diag(vcount(scenes)) - alpha * A)

# Compute the "broadcast centality vector" b, which is Q's row sums.
b <- Q %*% rep(1,vcount(scenes))
most.central.broadcasters <- round(sort(abs(b),decreasing=TRUE),3)
names(most.central.broadcasters) <- V(scenes)[order(b,decreasing=TRUE)]$name

# Compute the "receive centality vector" r, which is Q's col sums.
r <- t(Q) %*% rep(1,vcount(scenes))
most.central.receivers <- round(sort(abs(r),decreasing=TRUE),3)
names(most.central.receivers) <- V(scenes)[order(r,decreasing=TRUE)]$name

cat("\nKatz centrality, alpha = ", round(alpha,4), ":\n", sep="")
cat("Broadcasters:\n")
print(sort(most.central.broadcasters,decreasing=TRUE))
cat("Receivers:\n")
print(sort(most.central.receivers,decreasing=TRUE))
