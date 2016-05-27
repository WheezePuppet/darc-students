
# Community detection.

library(igraph)

source("loadSW.R")

plot(fastgreedy.community(scenes),scenes,main="scenes")
readline()
plot(fastgreedy.community(touches),touches,main="touches")
readline()
plot(fastgreedy.community(as.undirected(mentions)),mentions,main="mentions")
readline()
plot(fastgreedy.community(as.undirected(dialog)),dialog,main="dialog")
