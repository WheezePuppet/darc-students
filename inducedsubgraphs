induced.subgraph.at.time.t <- function(g, N, t){
  tweeters <- N[[t]]
  raw.graph <- induced_subgraph(g, which(V(g)$name %in% tweeters), impl= c("auto"))
  return (raw.graph)
}

induced.subgraph.up.to.time.t <- function(g, N, t){
  tweeters <- unique(unlist(N[1:t]))
  cooked.graph <- induced_subgraph(g, which(V(g)$name %in% tweeters), impl = c("auto"))
  return (cooked.graph)
}
