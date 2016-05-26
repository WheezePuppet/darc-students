
library(igraph)


plot.sw.graph <- function(sw.graph, title="", edge.arrow.size=.5, 
    vertex.size=20, edge.lty="solid") {

    V(sw.graph)$color <- 
        ifelse(characters$species == "human", "yellow",
        ifelse(characters$species == "alien", "green",
        ifelse(characters$species == "droid", "grey",
        ifelse(characters$species == "wookie", "brown", "white"))))

    plot(sw.graph, edge.arrow.size=edge.arrow.size, 
        vertex.size=vertex.size,
        edge.lty=edge.lty,
        edge.width=sqrt(E(sw.graph)$weight),
        layout=layout.fruchterman.reingold(
            sw.graph,weights=E(sw.graph)$weight^.4),
        main=title)
}

main <- function() {
    source("loadSW.R")
    plot.sw.graph(scenes,"Scenes in common")
    readline("Press ENTER.")
    plot.sw.graph(touches,"Physical contacts")
    readline("Press ENTER.")
    plot.sw.graph(mentions,"Mentions (in dialog)",edge.lty="dashed")
    readline("Press ENTER.")
    plot.sw.graph(dialog,"Direct address (in dialog)")
}
