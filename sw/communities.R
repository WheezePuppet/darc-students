
# Community detection.

library(igraph)

source("loadSW.R")

plot(fastgreedy.community(scenes),scenes,main="fastgreedy.community(scenes)")
