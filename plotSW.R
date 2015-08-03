
library(igraph)

characters <- read.csv("chars.csv",header=TRUE)

plot.sw.graph <- function(csv.filename, title, directed=FALSE,
    multiplicity.column.name=NULL, edge.arrow.size=.5, vertex.size=20,
    edge.lty="solid") {

    the.data.frame <- read.csv(csv.filename,header=TRUE)
    multigraph <- graph.data.frame(the.data.frame, 
        directed=directed, vertices=characters)

    if (!is.null(multiplicity.column.name)) {
        E(multigraph)$weight <- the.data.frame[,multiplicity.column.name]
    } else {
        E(multigraph)$weight <- 1
    }

    simplified.graph <- simplify(multigraph)

    V(simplified.graph)$color <- 
        ifelse(characters$species == "human", "yellow",
        ifelse(characters$species == "alien", "green",
        ifelse(characters$species == "droid", "grey",
        ifelse(characters$species == "wookie", "brown", "white"))))

    plot(simplified.graph, edge.arrow.size=edge.arrow.size, 
        vertex.size=vertex.size,
        edge.lty=edge.lty,
        edge.width=sqrt(E(simplified.graph)$weight),
        layout=layout.fruchterman.reingold(
            simplified.graph,weights=E(simplified.graph)$weight^.4),
        main=title)
}


plot.sw.graph("scenesAll.csv","Scenes in common",directed=FALSE)
readline("Press ENTER.")
plot.sw.graph("touches.csv","Physical contacts",directed=FALSE)
readline("Press ENTER.")
plot.sw.graph("mentions.csv","Mentions (in dialog)",
    multiplicity.column.name="numMentions",directed=TRUE,edge.lty="dashed")
readline("Press ENTER.")
plot.sw.graph("dialogAll.csv","Direct address (in dialog)",
    multiplicity.column.name="numUtterances",directed=TRUE)
