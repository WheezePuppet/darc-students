#Computes the graph invariants for a list of graphs

library(igraph)

#Returns the number of components for a list of graphs.

num.components <- function(graph){
	
	#Was not sure which one we wanted.....So I did both
	
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



#Returns the clustering coefficient for a list of graphs.
#The global cluster coefficient is the number of closed triplets (or 3 x triangles) over the total number of triplets (both open and closed)
#Returns a value from 0(meaning no vertex that is connected to Vi connects to any other vertex that is connected to Vi) to 1(meaning every neighbor connected to Vi is also connected to every other vertex within the negihborhood). If there are no connected triples, returns NaN.

cluster.coeff <- function(graph){
	
	#Global or local??

	#Clustering Coefficient for one graph
	#trans <- transitivity(graph, type="global")
	
	#Clustering Coefficients for a list of graphs	
	trans <- c()
	for (i in 1:length(graph)){
		coefficients.of.graph <- transitivity(graph[[i]], type="global")
		trans <- c(trans, coefficients.of.graph)
	}
	return (trans)
}
