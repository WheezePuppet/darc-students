
# Generate a random graph with the same degree sequence as the SW scenes
# graph, and look at its properties.

library(igraph)

source("loadSW.R")

degs <- degree(scenes)

scenes.not <- degree.sequence.game(degs,method="vl")

par(mfrow=c(1,2))
plot(scenes,vertex.label="",main="True scenes data")
plot(scenes.not,vertex.label="",main="Artificial data")

cat("Diameters:",diameter(scenes),"vs.",diameter(scenes.not),"\n")
cat("Global CC:",transitivity(scenes),"vs.",transitivity(scenes.not),"\n")
cat("Avg path length:",average.path.length(scenes),"vs.",
    average.path.length(scenes.not),"\n")

X11()
plot(sort(transitivity(scenes,type="local")),col="orange",pch=20)
points(sort(transitivity(scenes.not,type="local")),col="grey",pch=20)

