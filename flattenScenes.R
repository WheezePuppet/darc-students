
flattenScenes <- function(in.file, out.file=paste0(in.file,".flat")) {

    lines <- strsplit(readLines(in.file),",")
    flattened.df <- do.call("rbind",
        lapply(lines, function(line) {
                loc <- line[length(line)]
                chars <- line[-length(line)]
                if (length(chars) >= 2) {
                    df <- data.frame(t(combn(chars,2)),loc=loc)
                    colnames(df) <- c("char1","char2","loc")
                    return(df)
                } else {
                    return(NULL)
                }
            }
        )
    )
    write.csv(flattened.df, file=out.file, row.names=FALSE)
}

characters <- read.csv("chars.csv",header=TRUE)
scenes <- graph.data.frame(
    read.csv("scenes.csv.flat",header=TRUE),
    directed=FALSE, vertices=characters)
V(scenes)$color <- 
    ifelse(characters$species == "human", "yellow",
    ifelse(characters$species == "alien", "green",
    ifelse(characters$species == "droid", "grey",
    ifelse(characters$species == "wookie", "brown", "white"))))
plot(scenes, edge.arrow.size=.5, vertex.size=22)
