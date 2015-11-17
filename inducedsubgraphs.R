library(igraph)

induced.subgraph.at.time.t <- function(g, N, t){
  tweeter <- N$graph[[t]]$tweeter
  followers <- N$graph[[t]]$followers
  timestamp <- N$attribute[[t]]$tweet.date
  #raw.graph <- induced_subgraph(g, which(V(g)$name %in% tweeters), impl= c("auto"))
  vec<- c(pwners$graph[[t]]$tweeter, pwners$graph[[t]]$followers)
  raw.graph <- induced_subgraph(g, vec, impl=c("auto"))
 
  #Bad way
  #raw.graph$attr <- timestamp
  #Good way
  #If we want timestamp of the tweeter's first tweet
  raw.graph <- set_graph_attr(raw.graph, "Time", timestamp)

  #Bad way
  #V(raw.graph)$name <- tweeter
  #Good way
  raw.graph <- set_graph_attr(raw.graph, "Tweeter", tweeter)

  return (raw.graph)
}

induced.subgraph.up.to.time.t <- function(g, N, t){
  tweeters<- unique(unlist(N$g[1:t]))
  timestamps <- N$attribute[[t]]$tweet.date
  cooked.graph <- induced_subgraph(g, which(V(g)$name %in% tweeters), impl = c("auto"))
  cooked.graph$attr <- timestamps
  V(cooked.graph)$name <- tweeters[]
  return (cooked.graph)
}


#N <- list(c("Leia", "Han", "Wedge"), c("Leia", "Emperor", "Luke"), c("Lando", "Han", "Luke"), c("Jerjerrod", "Greedo", "Lando", "Han", "Yoda"), c("Yoda", "Needa", "Luke", "Vader", "Biggs", "Owen"))
#plot(induced.subgraph.up.to.time.t(mentions, N, 2))
#plot(induced.subgraph.at.time.t(mentions, N, 5))
