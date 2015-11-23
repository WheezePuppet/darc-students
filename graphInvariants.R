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
#Value returned is from 0 to 1. If there are no connected triples, returns NaN.
cluster.coeff <- function(graph){
	
	#Was not sure which one we wanted.....So I did both

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
