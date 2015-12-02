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
# Aaron TODO: change this to work with degree.distribution() function.
#  add a "mode" argument to this function and pass it through to
#  degree.distribution().
#    sequences[i]<-degree.distribution(out.degree.graph(graph)[i],in.degree.graph(graph)[i])
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
		for (j in 1:length(N$graph)){
			tweeters <- N$graph[[j]]$tweeter
			tweeters.vec <- c(tweeters.vec,tweeters)
		}
		#The first tweeter is NA
		outside <- c(NA)
        subgraphs <- induced.subgraphs.up.to.all.times.t(curr.graph,N)
		for (j in 2:length(tweeters.vec)){
			x <- toString(tweeters.vec[[j]])
			result <- exists.in.graph(x, subgraphs[[j]])
            # Dave is UUUUgly but we still looooove him
            outside <- c(outside, as.integer(!result))
		}
        list.of.vecs[[i]] <- outside
	}
	return (list.of.vecs)
}


# THE MASTER FUNCTION
# Hannah TODO:
# Given a search string (like "pwn") return a list of vectors of graph invariants
# Each element in the list is named:
#    num.components
#    diameter
#    outside.tweeter
#    --- the weird one is Aaron's degree distribution thing, since that's a
#        vector for each of the 100.
#    etc.
# Hannah and Liv TODO: make this work with memento pattern, which means: 
#    there's a "cache" directory with a bunch of files, each of which:
#        the name of the file is the search string and n, concatenated with
#           underscore.
#        the contents of the file is a binary RData file with one variable
#           named f.search.string. This variable is a list of n unnamed 
#           elements. Each element is a list with three named elements:
#           user -- a character string of the kth user ID to tweet that string
#           date -- the date of the kth tweet (POSIXct)
#           followers -- a vector of character strings representing the user
#              IDs of the tweeters who follow user k
# Example:
#     filename: pwn_10.RData
#     contents: a binary RData file with a variable called f.pwn_10, which is
#         a list of 10 elements as described above.
gather.all.invariants <- function(search.string, n=10) {

    # Check "cache" directory. (Create if it doesn't exist.)
    # In cache directory, look for file called "search.string_n.RData" 
    # (e.g., "pwn_10.RData")
    # If it doesn't exist, call the time-consuming Liv stuff and save a file
    # by that name with that time-consuming Liv result, named appropriately.
    # If it does, sweeeeet, load() the file and use it.

    # Think deeply about this line. Wow.
    get(paste0("f.",search.string,"_",n)) -> everyone

    # And now, we have a plain ol' variable called everyone.

    get.everyone(search.string,n) -> everyone
    make.medium.graph(the.everyone.for.this.search.string) -> 
        the.medium.graph.

    # fill in this code to call all the graph invariant functions

    # Return a list with one component for each graph variant. (Each such
    # component is a list n long.
}


