#Computes the graph invariants for a list of graphs

library(igraph)

source("inducedsubgraphs.R")

#Returns the number of components for a list of graphs.

num.components <- function(graph){
	
	#Components of one graph
	#components.of.graph <- components(graph, mode="weak")
	#num <- components.of.graph$no

	#Components of a list of graphs
	num <- c()
	for (i in 1:length(graph)){
		components.of.graph <- components(graph[[i]], mode="weak")
		number <- components.of.graph$no
		num <- c(num, number)	
	}
	return (num)
}

#Pre- stuff needed for degree sequence
out.degree.graph<-function(graph){
  degrees<-vector(length=length(graph))
  for (i in 1:length(graph)){
    degrees[i]<-degree(graph[[i]], mode=c("out"), loops = FALSE)
  }
  return(degrees)
}

in.degree.graph<-function(graph){
  degrees<-vector(length=length(graph))
  for (i in 1:length(graph)){
    degrees[i]<-degree(graph[[i]], mode=c("in"), loops = FALSE)
  }
  return(degrees)
}

#Returns a list of degree sequences for a list of graphs
degree.sequence.graph<-function(graph){
  sequences<-vector(length=length(graph))
  for(i in 1:length(graph)){
    sequences[i]<-degree.sequence.game(out.degree.graph(graph)[i],in.degree.graph(graph)[i])
  }
  return (sequences)
}

#Returns vector of the diameters of a list of graphs

diameter.graph<-function(graph){
  diameters<-vector(length=length(graph))
  for(i in 1:length(graph)){
    diameters[i]<-diameter(graph, directed = TRUE, unconnected = TRUE)
  }
  return (diameters)
}


#Returns the clustering coefficient for a list of graphs.
#The global cluster coefficient is the number of closed triplets (or 3 x triangles) over the total number of triplets (both open and closed)
#Returns a value from 0(meaning no vertex that is connected to Vi connects to any other vertex that is connected to Vi) to 1(meaning every neighbor connected to Vi is also connected to every other vertex within the negihborhood). If there are no connected triples, returns NaN.

#Takes two arguments: 1) a list of graphs 2)FALSE:if you want a coefficient of the entire graph(global), TRUE: if you want the mean of the coefficients of all the local triangles(local)
#Default does global

cluster.coeff <- function(graph, local=FALSE){
	
	#Clustering Coefficient for one graph
	#trans <- transitivity(graph, type="global")
	
	#Clustering Coefficients for a list of graphs	
	trans <- c()
	for (i in 1:length(graph)){
		if(!is.connected(graph[[i]])){
			trans <- c(trans, Inf)
		}else{
			if(local==FALSE){
				coefficients.of.graph <- transitivity(graph[[i]], type="global")
				trans <- c(trans, coefficients.of.graph)

			}else{
				#Removes NaN values
				coefficients.of.graph <- transitivity(graph[[i]], type="local")
				coefficients.of.graph <- coefficients.of.graph[!coefficients.of.graph %in% NaN]
				b <- mean(coefficients.of.graph)
				trans <- c(trans, b)
			}
		}
	}
	return (trans)
}

#Returns the diameter for each graph in a list.

#Default does directed=FALSE
graph.diameter<- function(graphs, dir=FALSE){
	diam <- c()
	for (i in 1:length(graphs)){
		if(dir==TRUE){
			diam.of.graph <- diameter(graphs[[i]], directed=TRUE, unconnected=FALSE)
			diam <- c(diam, diam.of.graph) 
		}else{
			diam.of.graph <- diameter(graphs[[i]], directed=FALSE, unconnected=FALSE)
			diam <- c(diam, diam.of.graph) 
		}
	}
	return (diam)
}

#If tweeter t is in the subgraph of 1:t-1, value is 0. If not, value is 1.

#Need to pass in a list of graphs and a list of their N(everyone) values
outside.tweeter <- function(graphs, list.of.N){
	list.of.vecs <- list()
	for (i in 1:length(graphs)){
		tweeters.vec <- c()
		curr.graph <- graphs[[i]]
		N <- list.of.N[[i]]
		#because we've only asked for up to 10, will change to 100
		for (i in 1:length(N$graph)){
			tweeters <- N$graph[[i]]$tweeter
			tweeters.vec <- c(tweeters.vec,tweeters)
		}
		#The first tweeter is NA
		outside <- c(NA)
        #subgraph == first graph
		for (i in 2:length(tweeters.vec)){
			#if (i>1)
                 #subgraph <- add.to.graph.thing(subgraph,N$graph[[i]],N$attribute[[i]])
            subgraph <- induced.subgraph.up.to.time.t(curr.graph,N,i-1)
			x <- toString(tweeters.vec[[i]])
			result <- exists.in.graph(x, subgraph)
			if (result){
				outside <- c(outside, 0L)
			}else{
				outside <- c(outside, 1L)
			}
		}
	list.of.vecs <- list(list.of.vecs, outside)
	}
	return (list.of.vecs)
}
