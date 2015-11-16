
source("getTwitterUsers.R")

library(igraph)

#The Vertex attributes must have a $name
exists.in.graph <- function(user.id, twitter.graph){
  for(x in 1:length(V(twitter.graph))){
    answer=FALSE
    if(user.id==V(twitter.graph)$name[x]){
      return(TRUE)
      }
  }
  return(FALSE)
}

# Returns a list with 2 elements, one of which is graph and one is attributes
#   graph is a list of n elements, each of which is a list with two elements:
#   The first element of the kth element is the userid of the kth tweeter.
#   The second element of the kth element is a vector of the kth tweeters'
#     followers.
# first.user.ids is an optional argument, containing a vector of the first
#   several user IDs who tweeted a certain search string. n and search.string
#   are ignored in this case. Otherwise, get.first.user.ids() will be called 
#   (from Liv) live.
get.N <- function(search.string, n=100, first.user.ids){
  #gets all user ids of the first n tweeters (from Liv)
  if (missing(first.user.ids)) {
    cat("Asking Liv for the first",n," tweeters of \"", search.string, "\"\n")
    first.user.ids <- get.first.user.ids(search.string,n)
  }
  uids <- unique(first.user.ids)

  #get followers for each tweet.ids
  everyone<-vector("list",n)
  attr<-vector("list",n)
  for(x in 1:length(uids)){
    cat("Processing userid",x,"...\n")
    user<-get.user.info(uids[x])
    # (Do the unique here because we *need* the first element in everyone[[x]]
    # to be this tweeter's userid, and we feel nervous about unique() 
    # preserving the order.)
    fol<-unique(user$followerIDs)
    everyone[[x]]<-list(tweeter=uids[x],followers=fol)
    ####### WHERE DO I FIND TIMESTAMP IN THE CODE???
    attr[[x]]<-list(tweet.date=x)
  }
  new_everyone<-list(graph=everyone,attribute=attr)
  return(new_everyone)
}

# have NOT changed this to accept the new.everyone
build.big.graph <- function(everyone){
  vertices<-unique(unlist(everyone[1]))
  big.graph<-make_empty_graph(n = length(vertices), directed = TRUE) #creating empty graph
  #adding everyone to the graph as vertecies
  V(big.graph)$name<-vertices

  # edge.thing is a vector of even length which contains pairs of vertices
  # corresponding to the edges, all in a row.
  # For instance, if we want to add the edges 1->2, 3->2, 4->5, edge.thing
  # will be the vector c(1,2,3,2,4,5).
  edge.thing <- vector()
  #adding the edges to the graph
  fol <- NULL
  for(v1 in 1:length(vertices)){
      #getting the followers of each person in vertices storing in fol
      user<-get.user.info(vertices[v1])
      fol<-c(fol,user$followers)
      #intersecting followers with vertices
      key_fol<-intersect(vertices,fol)
      if(length(key_fol)==0){
        #figure out what to do here
      }
      for(v2 in 1:length(key_fol)){
        #add_edges(big.graph, c(vertices[v1], vertices[v2])) -> big.graph
        edge.thing <- c(edge.thing, vertices[v1], vertices[v2])
      }
      #setting the date for the verticies
      # Stephen: we don't need to do this, since we only want a graph 
      # attribute for each of Hannah's cooked graphs.
      #for(x in 1:length(tweet.ids)){
      #  getdates<-get.tweet.info(tweet.ids[x])
      #  set.graph.attribute(big.graph,date,index=V(tweet.ids[x]), getdates$date)
      #}
  }
  big.graph <- add_edges(big.graph, edge.thing)
  return(big.graph)
}

make.medium.graph <- function(everyone){
  vertices<-everyone$graph
  medium.graph<-make_empty_graph(n = length(unlist(vertices)), directed = TRUE) #creating empty graph
  #adding everyone to the graph as vertecies
  V(medium.graph)$name<-unique(unlist(vertices))
  
  # edge.thing is a vector of even length which contains pairs of vertices
  # corresponding to the edges, all in a row.
  # For instance, if we want to add the edges 1->2, 3->2, 4->5, edge.thing
  # will be the vector c(1,2,3,2,4,5).
  edge.thing <- vector()
  #adding the edges to the graph
  for(v1 in 1:length(vertices)){
    for(v2 in 1:length(vertices[[v1]])){
      edge.thing <- c(edge.thing, vertices[v1]$tweeter, vertices[[v1]][v2])
    }
    # attribute for each of Hannah's cooked graphs.
  }
  medium.graph<- add_edges(medium.graph, edge.thing)
  return(medium.graph)
}
