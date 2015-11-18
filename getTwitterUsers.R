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
  counter <- 0 #if you want, you can manually change this if, say, you already have the first 50 and you want the next 50
  users <- list() #changed it to a list so I could filter integers
  unique.users <- 0 #we want this to match [quantity] in the end

  cat("So far, we have",length(users),"unique users...\n")
  while(length(unique(users)) < quantity) {
    first <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",counter*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))$response$list$trackback_author_nick
    second <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",counter*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))$response$list$trackback_author_nick
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
      counter <- (counter + 1) #goes to next page of results
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

#alright, let's see how this works…
#this will return a list, 100-long when unique'd by name, of the first hundred (repeated) user IDs
#each entry in the list (named for the user ID) is its own list, the first element being $user, the second element
#being $date (which I may need to format differently?), and the third being $followers (a vector of up to 25k friend IDs)
get.first.results <- function(search.string, quantity = 100) {
  search.string <- ifelse(grepl("#",search.string),paste0("23",substring(search.string,2)),search.string) #formats query
  counter <- 0 #while() loop counter
  beginning <- 1 #a starting point for each subsequent [counter]
  users <- list() #initially the usernames — later the user IDs
  tweet.dates <- c() #POSIX dates
  rem <- c() #vector of protected/404 users be removed
  while(length(unique(users)) < quantity) { #main loop
    first <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",counter*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))
    first.usernames <- first$response$list$trackback_author_nick
    first.dates <- first$response$list$trackback_date #calls manual API once…
    second <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",counter*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))
    second.usernames <- second$response$list$trackback_author_nick
    second.dates <- second$response$list$trackback_date #…and twice
    #this collects usernames/dates and merges them with each [counter]
    users <- c(users,as.list(combine(first.usernames,second.usernames)))
    tweet.dates <- unique(c(tweet.dates,combine(first.dates,second.dates)))
    #see [combine()] for details
    if(beginning > length(unique(users))) {
      break() #breaks if our list of users doesn't increase
    } else {
      names(users)[beginning:length(users)] <- users[beginning:length(users)] #names all entries in [users] after themselves
      if(length(rem) > 0) {
        rem <- unique(rem) #gets rid of repeated usernames to be removed each [counter]
        names(users[beginning:length(users)])[c(which(users[beginning:length(users)]%in%rem))] <- rep("~") #marks usernames to be removed
      } #basically, I use "~" to identify users we can't use
    }
    
    for(i in beginning:length(users)) { #where most of the important stuff goes on
      if(names(users)[i] != "~" && length(users[[i]]) == 1) { #only looks at usernames that haven't been marked with "~"
        if(curl_fetch_memory(paste0("twitter.com/",users[[i]]))$status_code==404) { #checks for 404 (deleted/suspended user) error
          rem <- c(rem,users[[i]]) #adds these names for later removal
          names(users)[grep(names(users)[[i]],names(users))] <- rep("~") #marks all matching names with "~"
        } else {
          user.info <- getUser(users[[i]]) #calls twitteR to get user info
          if(user.info$protected) { #same thing as above, but for protected users
            rem <- c(rem,users[[i]])
            names(users)[grep(names(users)[[i]],names(users))] <- rep("~")
          } else { 
            names(users)[grep(names(users)[[i]],names(users))] <- rep(user.info$id) #converts all atching usernames to IDs
            if(counter > 0 && names(users)[i]%in%names(users)[1:(i - 1)]) { #checks for any matching usernames in previous [counter]
              users[[i]] <- (users[names(users)[i]%in%names(users)[1:(i - 1)]])[[1]] #if successful, clones that information into this entry
            } else {
              users[[i]] <- user.info$getFollowerIDs() #otherwise, calls twitteR to get follower IDs
            } #and this next bit clones the friend ID vector onto any subsequent matching user ID entries
            users[i:length(users)] <- replace(users[i:length(users)],which(names(users[i:length(users)])==names(users)[i]),users[i])
          }
        }
      }
    } #and so ends the for() loop that does most of the work
    
    if("~"%in%names(users)) { #only does this if there's a user marked with "~"
      tweet.dates <- tweet.dates[-which(names(users)=="~")] #removes the [tweet.dates] values matching those in [users]
      users <- users[-which(names(users)=="~")] #removes all users marked with "~"
    }
    
    while(length(tweet.dates) > length(users)) { #ensures [tweet.dates] isn't longer than [users]
      tweet.dates <- tweet.dates[-length(tweet.dates)] #I don't realistically ever expect this to happen…but hey, just in case
    }
    
    if(length(unique(users)) >= quantity) { #this part only happens when you're essentially done
      while(length(unique(users)) > quantity) {
        users <- users[-length(users)] #trims the excess at the end so we only get 100 unique user IDs
        tweet.dates <- tweet.dates[-length(tweet.dates)] #the same for the dates
      }
      tweet.dates <- as.POSIXct(tweet.dates,origin="1970-01-01",tz="GMT") #converts the date to be readable
      #I may need to tweak this slightly?
      for(n in 1:length(users)){ #final formatting
        users[[n]] <-  list(names(users)[[n]],tweet.dates[n],users[[n]]) #upgrades [users] from list of vectors, to list of lists
        names(users[[n]]) <- c("userID","date","followers") #names these sublists
      } #end of the "essentially done" if()
    } else { #this part is for when you don't have enough (unique) user IDs and have to loop through the code again
      counter <- (counter + 1) #increases [counter]
      beginning <- (length(users) + 1) #bumps up the start point for the next iteration to save on unnecessary loops
    }
  }
  return(users)
} #you're welcome
