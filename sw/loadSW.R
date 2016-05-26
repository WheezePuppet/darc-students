
library(igraph)

characters <- read.csv("chars.csv",header=TRUE)

construct.sw.graph <- function(csv.filename, directed=FALSE,
    multiplicity.column.name=NULL) {

    the.data.frame <- read.csv(csv.filename,header=TRUE)
    multigraph <- graph.data.frame(the.data.frame, 
        directed=directed, vertices=characters)

    if (!is.null(multiplicity.column.name)) {
        E(multigraph)$weight <- the.data.frame[,multiplicity.column.name]
    } else {
        E(multigraph)$weight <- 1
    }

    simplified.graph <- simplify(multigraph)
    return(simplified.graph)
}


scenes <- construct.sw.graph("scenesAll.csv",directed=FALSE)
touches <- construct.sw.graph("touchesAll.csv",directed=FALSE)
dialog <- construct.sw.graph("dialogAll.csv",
    multiplicity.column.name="numUtterances",directed=TRUE)
mentions <- construct.sw.graph("mentionsAll.csv",
    multiplicity.column.name="numMentions",directed=TRUE)
