#Computes the graph invariants for a list of graphs

library(igraph)

#Computes the number of components in the graph
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
