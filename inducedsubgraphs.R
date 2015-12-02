library(igraph)

#Creates a subgraph of graph g using the vertices from N at tweeter #t
induced.subgraph.at.time.t <- function(g, N, t){
	tweeter <- N$graph[[t]]$tweeter
	followers <- N$graph[[t]]$followers
	timestamp <- N$attribute[[t]]$tweet.date
 	vec<- c(tweeter, followers)

	#Makes a subgraph
	raw.graph <- induced_subgraph(g, vec, impl=c("auto"))
 
	#Timestamp of the t'th tweeter's tweet
  	raw.graph <- set_graph_attr(raw.graph, "Time", timestamp)

  	#Twitter ID of the t'th tweeter
  	raw.graph <- set_graph_attr(raw.graph, "Tweeter", tweeter)

  	return (raw.graph)
}


#Creates a subgraph of graph g using the vertices from N from tweeter 1 through t
induced.subgraphs.up.to.all.times.t <- function(aarons.alltime.graph, N){

  	tweeter<- c()
  	followers<- vector("list")
  	timestamp<- c()

    cooked.graphs <- vector("list", length=length(N$graph))
	cooked.graphs[[1]] <- induced_subgraph(aarons.alltime.graph,
        unique(c(N$graph[[1]]$tweeter,
             N$graph[[1]]$followers)), impl=c("copy_and_delete"))
    
    for (num in 2:length(N$graph)) {
        blah <- induced_subgraph(aarons.alltime.graph,
            unique(c(N$graph[[num]]$tweeter,
                 N$graph[[num]]$followers)), impl=c("copy_and_delete"))
        cooked.graphs[[num]] <- union(blah, cooked.graphs[[num-1]])
        cooked.graphs[[num]] <- set_graph_attr(cooked.graphs[[num]], "Time",
            N$attribute[[num]]$tweet.date)
        cooked.graphs[[num]] <- set_graph_attr(cooked.graphs[[num]], "Tweeter",
            N$graph[[num]]$tweeter)
    }

  	return (cooked.graphs)
}

