library(igraph)
library(lubridate)
library(animation)

# find the springer book that describes this stuff
# version 1:
#   graph is static, choose random edge instead of random vertex
#    (Dave likes first choosing token or not, then choosing edge, but maybe
#    that's not as real-life.)
# version 2:
#   graph still static, but you can send to a non-edge
# version 3:
#   keep track of how many times you've seen the token, transmit more likely
#    if you've seen it a lot.
# version 4:
#   there's a constant number of tokens
# version 3:
#   graph is dynamic
#   
## Build a graph
## generate edge observations as quads:
##
## t v1 v2 b

## case 1: token b=1 starts at a vertex
## prob of b=1 fixed at p, conditional on a vertex
## having seen a 1, otherwish p=0.
##
## case 2: no condition on vertex having seen a 1.

## build an sbm graph

seed <- 323082
set.seed(seed)
K <- 20         # K: number of blocks
ni <- 1000      # ni: number of agents per block
    K <- 6
    ni <- 10
group.vertex.ids <- lapply(1:K, function(g.num) { ((g.num-1)*ni)+(1:ni) })
poff <- 10/ni   # poff: min prob that nodes in same block will be connected
pin <- 10*poff  # pin: max prob that nodes in same block will be connected
    poff <- 0.9
    pin <- 0.9


P <- matrix(runif(K*K,0,.05),ncol=K)
diag(P) <- runif(K,poff,pin)
g <- sbm.game(n=K*ni,pref.matrix=P,block.sizes=rep(ni,K),
              directed=TRUE,loops=FALSE)
save.layout <- layout.auto(g)
V(g)$color <- "blue"

save(K,ni,poff,pin,P,g,seed,file="simulationSD.RData")

edges <- get.edgelist(g)

## Initiating the stream with no signal

seed <- 9843579
set.seed(seed)
init_N <- 100

s <- today()
#times <- s+seconds(sort(rexp(init_N,.1)))
times <- s+seconds(cumsum(rexp(init_N,.1)))        # SD 'fixed'
samp <- sample(nrow(edges),init_N, replace=TRUE)   # SD added replacement
data <- data.frame(source=edges[samp,1],
						 destination=edges[samp,2],
						 token=rep(0,init_N),
						 time=times)

## insert one token in the graph
samp <- sample(nrow(edges),1)
seen_token <- c(edges[samp,])

M <- 10000        # M: number of simulated messages
    M <- 1000
s <- times[init_N]+seconds(rexp(M,.1))
data <- rbind(data,data.frame(
              source=rep(0,M),
				  destination=rep(0,M),
				  token=rep(0,M),
				  time=s))
data[init_N+1,c("source","destination","token")] <- c(edges[samp,],1)

P_new <- 0.1
P_token <- 0.1

ani.options(interval=.2)
ani.record(reset=TRUE)
for(i in 2:M){
   rval <- runif(2)
	## with probability P_token we will pass a token on
	token <- as.logical(rval[1]<=P_token)
	if(token){
		## choose one of the vertices that's seen a token
		## for the source
	   edge <- c(sample(seen_token,1),0)
	} else {
		# v <- setdiff(sample(vcount(g),length(seen_token)+1),seen_token)[1]
	   edge <- c(sample(vcount(g),1),0)
	}
	if(rval[2]<=P_new){
		## with probability P_new just choose a vertex
		## at random from the set of vertices
	   e1 <- sample(vcount(g),2)
		edge[2] <- e1[which(e1 != edge[1])[1]]
	} else {
		ind <- which(edges[,1] == edge[1])
		if(length(ind) == 0) {
			ind <- which(edges[,2] == edge[1])
			if(length(ind)==1) {
			   edge[2] <- edges[ind,1]
			} else {
				edge[2] <- edges[sample(ind,1),1]
			}
		} else {
			if(length(ind)==1) {
			   edge[2] <- edges[ind,2]
			} else {
				edge[2] <- edges[sample(ind,1),2]
			}
		}
	}
	if(token){
		seen_token <- union(seen_token,edge)
        V(g)[seen_token]$color <- "red"
        E(g)$color <- "grey"
        E(g)[edge[1] %->% edge[2]]$color <- "red"
        plot(g,layout=save.layout,vertex.size=10,mark.groups=group.vertex.ids)
        ani.record()
	}
	data[init_N+i,c("source","destination","token")] <- c(edge,token)
    if (i %% 10 == 0) cat(i,sum(data$token),length(seen_token),"\n")
}

saveHTML(ani.replay(),img.name="rumorProp")
save(data,file="dataSD.RData")
