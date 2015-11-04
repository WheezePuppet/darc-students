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

get.N <- function(search.string, n=100){
  #gets all user ids of the first n tweeters
  uid <- find.origninal.tweets(search.string,n)
  #get followers for each tweet.ids
  everyone<-vector("list",n)
  for(x in 1:length(uid)){
    user<-get.user.info(uid[x])
    fol<-user$followers
    everyone[[x]]<-c(uid[i],fol)
  }
  return(everyone)
}

build.big.graph <- function(everyone){
  #make N
  #make vertices
  #for each vertex in vertices
    #ask liv who follows him/her -> fol
    #intersect fol with vertices -> key_fol
    #####possible if statement needed for empty key_fol
    #for each follower in key_fol
      #add an edge from V1 to V2 in big graph
  #simplify(big graph)->big.graph
  #for each i in 1:99
    # call hannah's "induced_up_to_time_t
  vertices<-unique(unlist(everyone))
  big.graph<-make_empty_graph(n = length(vertices), directed = TRUE) #creating empty graph
  #adding everyone to the graph as vertecies
  V(big.graph)$name<-vertices
  #adding the edges to the graph
  for(x in 1:length(vertices)){
    #getting the followers of each person in verticies storing in fol
    user<-get.user.info(vertices[x])
    fol<-c(fol,user$followers)
  }
  #intersecting followers with vertices
  key_fol<-intersect(vertices,fol)
  if(length(key_fol)==0){
    #figure out what to do here
  }
  for(x in 1:length(key_fol)){
    #need to finish adding the edges
  }
  #setting the date for the verticies
  for(x in 1:length(tweet.ids)){
    getdates<-get.tweet.info(tweet.ids[x])
    set.graph.attribute(big.graph,date,index=V(tweet.ids[x]), getdates$date)
  }
}
