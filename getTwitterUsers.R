#From Liv:

library(httr)
library(stringi)
library(stringr)
library(curl)
library(twitteR)
source("twitterLogonInfo.R")
source("manualApi.R") #now incorporating Stephen's API
setup_twitter_oauth(key,secret,access_token,access_token_secret) #be sure to do this part!

# Returns the (repeated) first [quantity] user IDs. Just unique() it if you only want the unrepeated IDs.
# make sure to remember to add the ‘#’ symbol in search.string if you’re searching for a hashtag and not just a word.
get.first.user.ids <- function(search.string, quantity=100) {
  search.string <- ifelse(grepl("#",search.string),paste0("23",substring(search.string,2)),search.string)
  count <- 0 #if you want, you can manually change this if, say, you already have the first 50 and you want the next 50
  users <- list() #changed it to a list so I could filter integers
  unique.users <- 0 #we want this to match [quantity] in the end

  cat("So far, we have",length(users),"unique users...\n")
  while(length(unique(users)) < quantity) {
    first <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",count*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))$response$list$trackback_author_nick
    second <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",count*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))$response$list$trackback_author_nick
    users <- c(users,as.list(combine(first,second)))
    
    if(unique.users==length(unique(users))) { 
      break() #breaks if our list of users doesn't increase
    }
    
    #this part is for all those increadibly annoying people who decided to delete their twitter or set it to protected
    #I'm lookin' at you, @_ninjalove and @kathrinoutloud >:(
    cat("Dealing with annoying people like @_ninjalove...\n")
    for(i in (unique.users + 1):length(users)) { #only looks at new data
      cat("This is a generic bread crumb.\n")
      if(!is.numeric(users[[i]]) && !is.na(users[[i]])) { #reduces twitteR calls by only grabbing usernames—fails in the case of all-numeral usernames
        if(curl_fetch_memory(paste0("twitter.com/",users[[i]]))$status_code==404) { #filters out deleted users
          users[grepl(unlist(users)[[i]],unlist(users))] <- NA #replaces all deleted usernames with 'NA'
        } else {
          user.info <- getUser(users[[i]]) #calls twitteR here so I don't have to do it twice below
          users[grepl(unlist(users)[[i]],unlist(users))] <- ifelse(user.info$protected,NA,as.numeric(user.info$id)) #converts matching usernames to user ID/replaces protected usernames with 'NA'
        }
      }
    }
  
    users <- users[!is.na(users)] #removes all 'NA' values
    unique.users <- length(unique(users)) 
    
    if(unique.users > quantity) { #this only happens if we've overreached our goal
      while(length(unique(users)) > quantity) { #not perfect, but practical for our purposes
        users <- users[-length(users)] #trims the excess at the end so we only get [quantity] different users
      }
    } else {
      count <- (count + 1) #goes to next page of results
    }
  cat("So far, we have",length(users),"unique users...\n")
  }
  return(unlist(users))
} #tada!

#this function converts usernames to their matching user IDs and vice-versa
convert.user <- function(user) { #of course, I'm assuming you don't have someone with an all-numeral username
  if(suppressWarnings(is.na(as.numeric(user)))) { #determines if input is username or user ID
    conversion <- getUser(user)$id #converts username to user ID
  } else {
    conversion <- getUser(user)$screenName #converts user ID to username
  }
  return(as.character(conversion))
} #that's about it

#this will give you the username, user ID, follower IDs, and friend IDs of a user given a valid username or user ID
get.user.info <- function(user) {
  user.info <- getUser(user) #uses twitteR to get the info
  user.id <- user.info$id #grabs the user ID out of said info
  follower.list <- user.info$getFollowerIDs() #and the follower IDs
#  friend.list <- c(user.info$getFriendIDs()) #you get the picture
friend.list <- NULL
  user.info <- list(user.info,user.id,follower.list,friend.list) #a list of lists
  names(user.info) <- c("username","userID","followerIDs","friendIDs") #names lists for your convenience
  return(user.info)
} #the end

#this combines two similar vectors by finding the missing values and appending them in the correct places
combine <- function(first,second) {
  extra.num <- 0 #the length of [longer] changes, so this number anticipates that change for the for-loop
  if(length(first) < length(second)) { #figures out which vector is longer
    longer <- second
    shorter <- first
  } else {
    longer <- first
    shorter <- second
  }
  #if [shorter] is completely contained by [longer] then it returns the latter, otherwise it counts up the additional values from [shorter]
  ifelse(setequal(longer,union(longer,shorter)),return(longer),extra <- union(longer,shorter)[(length(unique(longer))+1):length(union(longer,shorter))])
  for(n in 1:length(extra)) { #it's all very confusing
    extra.num <- (length(grep(extra[n],shorter)) + extra.num)
  }
  #and this part combines everything magically
  for(i in 1:(extra.num + length(longer))) {
    if(longer[i] != shorter[i]) { #if they don't match
      if(is.element(longer[i],shorter[i:length(shorter)])) { #is this value even in [shorter]?
        allign <- grep(longer[i],shorter)[min(which(grep(longer[i],shorter)>i))]-1 #if so, find all the between values
        longer <- append(longer,shorter[i:allign],after=(i-1)) #and add them to [longer]
      } else if(is.element(shorter[i],longer[i:length(longer)])) { #or is the value in [longer]?
        allign <- grep(shorter[i],longer)[min(which(grep(shorter[i],longer)>i))]-1
        shorter <- append(shorter,longer[i:allign],after=(i-1)) #same thing as above, but vice-versa
      } else if(i <= length(shorter)) { #as long as we haven't exceeded [shorter] and the value is only in one vector
        longer <- append(longer,shorter[i],after=i) #adds it in to each
        shorter <- append(shorter,longer[i],after=(i-1))
      } else {
        shorter <- c(shorter,longer[i:length(longer)]) #grabs all values hanging off the end of [longer] when we've reached the end of [shorter]
      }
    }
  }
  return(longer)
} #I don't think I've put this much brain power into something since I tried to visualize the 4th dimension
