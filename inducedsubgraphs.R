library(igraph)

induced.subgraph.at.time.t <- function(g, N, t){
  tweeters <- N$g[[t]]
  attribute <- N$attr$timestamps
  raw.graph <- induced_subgraph(g, which(V(g)$name %in% tweeters), impl= c("auto"))
  raw.graph$attr <- attribute
  V$raw.graph$name <- tweeters[]
  return (raw.graph)
}

induced.subgraph.up.to.time.t <- function(g, N, t){
  tweeters<- unique(unlist(N$g[1:t]))
  attribute <- N$attr$timestamps
  cooked.graph <- induced_subgraph(g, which(V(g)$name %in% tweeters), impl = c("auto"))
  cooked.graph$attr <- attribute
  V$cooked.graph$name <- tweeters[]
  return (cooked.graph)
}


#N <- list(c("Leia", "Han", "Wedge"), c("Leia", "Emperor", "Luke"), c("Lando", "Han", "Luke"), c("Jerjerrod", "Greedo", "Lando", "Han", "Yoda"), c("Yoda", "Needa", "Luke", "Vader", "Biggs", "Owen"))
#plot(induced.subgraph.up.to.time.t(mentions, N, 2))
#plot(induced.subgraph.at.time.t(mentions, N, 5))
