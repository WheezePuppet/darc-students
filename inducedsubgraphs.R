library(igraph)

#Creates a subgraph of graph g using the vertices from N at tweeter #t
induced.subgraph.at.time.t <- function(g, N, t){
  tweeter <- N$graph[[t]]$tweeter
  followers <- N$graph[[t]]$followers
  timestamp <- N$attribute[[t]]$tweet.date
  vec<- c(pwners$graph[[t]]$tweeter, pwners$graph[[t]]$followers)
  raw.graph <- induced_subgraph(g, vec, impl=c("auto"))
 
  #Timestamp of the t'th tweeter's tweet
  raw.graph <- set_graph_attr(raw.graph, "Time", timestamp)

  #Twitter ID of the t'th tweeter
  raw.graph <- set_graph_attr(raw.graph, "Tweeter", tweeter)

  return (raw.graph)
}


#Creates a subgraph of graph g using the vertices from N from tweeter 1 through t
induced.subgraph.up.to.time.t <- function(g, N, t){
  tweeter<- c()
  followers<- vector("list")
  timestamp<- c()
  num <- t
  for (i in 1:num){
	tweeter[i] <- N$graph[[i]]$tweeter
	followers <- c(followers, N$graph[[i]]$followers)
 	timestamp[i] <- N$attribute[[i]]$tweet.date
  }
  followers <- unlist(followers)
  vec <- c(tweeter, followers)
  cooked.graph <- induced_subgraph(g, vec, impl=c("copy_and_delete"))

  #Do we actually want tweeter and tweet date for this function?
  
  return (cooked.graph)
}


#N <- list(c("Leia", "Han", "Wedge"), c("Leia", "Emperor", "Luke"), c("Lando", "Han", "Luke"), c("Jerjerrod", "Greedo", "Lando", "Han", "Yoda"), c("Yoda", "Needa", "Luke", "Vader", "Biggs", "Owen"))
#plot(induced.subgraph.up.to.time.t(mentions, N, 2))
#plot(induced.subgraph.at.time.t(mentions, N, 5))
