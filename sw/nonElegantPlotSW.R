
library(igraph)

characters <- read.csv("chars.csv",header=TRUE)

scenes.data.frame <- read.csv("scenesAll.csv",header=TRUE)
multigraph <- graph.data.frame(scenes.data.frame, 
    directed=FALSE, vertices=characters)
E(multigraph)$weight <- 1
scenes <- simplify(multigraph)

V(scenes)$color <- 
    ifelse(characters$species == "human", "yellow",
    ifelse(characters$species == "alien", "green",
    ifelse(characters$species == "droid", "grey",
    ifelse(characters$species == "wookie", "brown", "white"))))

plot(scenes, edge.arrow.size=.5,
    vertex.size=20,
    edge.lty="solid",
    edge.width=sqrt(E(scenes)$weight),
    layout=layout.fruchterman.reingold(
        scenes,weights=E(scenes)$weight^.4),
    main="Scenes in common")
readline("Press ENTER.")



dialog.data.frame <- read.csv("dialogAll.csv",header=TRUE)
multigraph <- graph.data.frame(dialog.data.frame, 
    directed=TRUE, vertices=characters)
E(multigraph)$weight <- dialog.data.frame[,"numUtterances"]
dialog <- simplify(multigraph)


V(dialog)$color <- 
    ifelse(characters$species == "human", "yellow",
    ifelse(characters$species == "alien", "green",
    ifelse(characters$species == "droid", "grey",
    ifelse(characters$species == "wookie", "brown", "white"))))

plot(dialog, edge.arrow.size=.5,
    vertex.size=20,
    edge.lty="solid",
    edge.width=sqrt(E(dialog)$weight),
    layout=layout.fruchterman.reingold(
        dialog,weights=E(dialog)$weight^.4),
    main=title)
readline("Press ENTER.")

