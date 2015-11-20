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
  counter <- 1 # if you want, you can manually change this if, say, you already have the first 50 and you want the next 50
  users <- list() #changed it to a list so I could filter integers
  unique.users <- 0 #we want this to match [quantity] in the end

  cat("So far, we have",length(users),"unique users...\n")
  while(length(unique(users)) < quantity) {
    first <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",(counter-1)*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))$response$list$trackback_author_nick
    second <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",(counter-1)*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))$response$list$trackback_author_nick
    users <- c(users,as.list(combine(first,second)))
    
    if(unique.users==length(unique(users))) { 
      break() #breaks if our list of users doesn't increase
    }
    
    #this part is for all those increadibly annoying people who decided to delete their twitter or set it to protected
    #I'm lookin' at you, @_ninjalove and @kathrinoutloud >:(
    cat("Dealing with annoying people like @_ninjalove...\n")
    for(i in (unique.users + 1):length(users)) { #only looks at new data
      cat("This is a generic bread crumb.\n")
      if(!is.numeric(users[[i]]) && !is.na(users[[i]])) { # reduces twitteR calls by only grabbing usernames
      user.url <- curl_fetch_memory(paste0("twitter.com/",users[[i]]))
        if(user.url$status_code==404 || grepl("suspended",user.url$url)) { # filters out deleted/suspended users
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
combine <- function(first, second) {
  extra.num <- 0 #the length of [longer] changes, so this number anticipates that change for the for-loop
  if(length(first) < length(second)) { #figures out which vector is longer
    longer <- second
    shorter <- first
  } else {
    longer <- first
    shorter <- second
  }
  #if [shorter] is completely contained by [longer] then it returns the latter, otherwise it counts up the additional values from [shorter]
  ifelse(setequal(longer,union(longer,shorter)), return(longer), extra <- union(longer,shorter)[(length(intersect(longer,shorter))+1):length(union(longer,shorter))])
  for(n in 1:length(extra)) { #it's all very confusing
    extra.num <- (length(grep(extra[n],longer)) + length(grep(extra[n],shorter)) + extra.num)
  }
  #and this part combines everything magically
  for(i in 1:(extra.num + length(longer))) {
  if(!setequal(longer,shorter)) { #the condition that they are not perfectly identical
      if(length(longer) <= i || length(shorter) <= i) { #a condition to prevent looping beyond the length of the vector
        if(longer[i] == shorter[i]) {
          ifelse(length(longer) >= length(shorter), return(longer), return(shorter)) #returns the longer vector
        }
      }
      if(longer[i] != shorter[i]) { #if they don't match
        if(is.element(longer[i],shorter[i:length(shorter)])) { #is this value even in [shorter]?
          allign <- grep(longer[i],shorter)[min(which(grep(longer[i],shorter)>i))]-1 #if so, find all the between values
          longer <- append(longer,shorter[i:allign],after=(i-1)) #and add them to [longer]
        } else if(is.element(shorter[i],longer[i:length(longer)])) { #or is the value in [longer]?
          allign <- grep(shorter[i],longer)[min(which(grep(shorter[i],longer)>i))]-1
          shorter <- append(shorter,longer[i:allign],after=(i-1)) #same thing as above, but vice-versa
        } else { #and this is for if both elements aren't in the other vector
          longer <- append(longer,shorter[i],after=i) #adds it in to each
          shorter <- append(shorter,longer[i],after=(i-1))
        }
      }
    }
  }
  return(longer)
} #I don't think I've put this much brain power into something since I tried to visualize the 4th dimension

# okay, this will return a list, 100-long when unique'd by name, of the first hundred (repeated) user IDs
# each entry in the list (named for the user ID) is its own list, the first element being $user, the second element being
# $date (which I may need to format differently?), and the third being $followers (a vector of up to 25k follower IDs)
get.first.results <- function(search.string, quantity = 100) {
  search.string <- ifelse(grepl("#",search.string),paste0("23",substring(search.string,2)),search.string) #formats query
  counter <- 1 # while() loop counter
  beginning <- 1 #a starting point for each subsequent [counter]
  users <- list() #initially the usernames — later the user IDs
  tweet.dates <- c() #POSIX dates
  rem <- c() # vector of protected/404 users be removed, which I like to keep as a global variable
  print("Starting…")
  while(length(unique(users)) < quantity) { #main loop
    first.call <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",(counter-1)*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))
    first.info <- first.call$response$list$trackback_date #calls manual API once…
    names(first.info) <- first.call$response$list$trackback_author_nick
    print("First call done")
    second.call <- make.manual.twitter.api.call(paste0("otter.topsy.com/search.json?q=%",search.string,"%20-rt&window=a&type=tweet&sort_method=-date&perpage=",quantity,"&offset=",(counter-1)*quantity,"&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/"))
    second.dates <- second.call$response$list$trackback_date #…and twice
    names(second.info) <- second.call$response$list$trackback_author_nick
    print("Second call done") # I just stuck the names onto the dates so I wouldn't have to short them twice
    
    all.info <- combine(first.info,second.info) #t his collects usernames/dates and merges them every [counter]
    tweet.dates <- c(tweet.dates,unname(all.info)) # and now it take the dates off
    users <- c(users,as.list(names(all.info))) # see the combine() function for details
    print("Merged all the info")
    
    if(beginning > length(users)) {
      print("Oops")
      return(users) # ends if the list isn't getting any bigger
    } else {
    print("We're still growing")
      names(users)[beginning:length(users)] <- users[beginning:length(users)] #names all entries in [users] after themselves
      if(length(rem) > 0) {
        rem <- unique(rem) #gets rid of repeated usernames to be removed each [counter]
        names(users[beginning:length(users)])[c(which(users[beginning:length(users)]%in%rem))] <- rep("~") #marks usernames to be removed
      } #basically, I use "~" to identify users we can't use
    }
    
    for(i in beginning:length(users)) { #where most of the important stuff goes on
      if(names(users)[i] != "~" && length(users[[i]]) <= 1) { # only looks at usernames that haven't been marked with "~"
        print(paste(i,"is unique"))
        if(length(users[[i]])==0) {
          users[[i]] <- names(users[i]) # this catches any list entries that have no sub-lists
          print(paste(i,"was empty"))
        }
        user.url <- curl_fetch_memory(paste0("twitter.com/",users[[i]]))
        if(user.url$status_code==404 || grepl("suspended",user.url$url)) { # checks for 404 (deleted/suspended user) error
          rem <- c(rem,users[[i]]) #adds these names for later removal
          names(users)[grep(names(users)[[i]],names(users))] <- rep("~") #marks all matching names with "~"
          print(paste(i,"gets ~"))
        } else {
          user.info <- getUser(users[[i]]) #calls twitteR to get user info
          print("Called twitteR")
          if(user.info$protected) { #same thing as above, but for protected users
            rem <- c(rem,users[[i]])
            names(users)[grep(names(users)[[i]],names(users))] <- rep("~")
            print(paste(i,"gets ~"))
          } else { 
            names(users)[grep(names(users)[[i]],names(users))] <- rep(user.info$id) #converts all atching usernames to IDs
            print(paste("Renamed",i))
            if(counter > 1 && names(users)[i]%in%names(users)[1:(i - 1)]) { # checks for any matching usernames in previous [counter]
              users[[i]] <- (users[names(users)[i]%in%names(users)[1:(i - 1)]])[[1]] #if successful, clones that information into this entry
            } else {
              print("Getting rate limit…")
              wait <- getCurRateLimitInfo()[32,] # gets rate limit
              print(paste0(wait[1,3],"/",wait[1,2]))
              if(wait[1,3]==0) { # if it hits 0…
                print(paste("Waiting until",wait[1,4])) # …then it sleeps until the 15 minute interval ends
                Sys.sleep(ifelse(as.POSIXct(wait[1,4])-as.POSIXct(Sys.time()) > 15,1,61)*(as.POSIXct(wait[1,4])-as.POSIXct(Sys.time())))
              } # it takes a while, considering we want 100 of these things
              users[[i]] <- user.info$getFollowerIDs() #otherwise, calls twitteR to get follower IDs
              print(paste("Gave",length(users[[i]]),"followers to",i))

            } # and this next bit clones the follower ID vector onto any subsequent matching user ID entries
            users[i:length(users)] <- replace(users[i:length(users)],which(names(users[i:length(users)])==names(users)[i]),users[i])
            print("Cloned followers")
          }
        }
      }
    } #and so ends the for() loop that does most of the work
    print(paste("Try number",counter,"nearly done"))
    
    if("~"%in%names(users)) { #only does this if there's a user marked with "~"
      print("Getting rid of ~")
      tweet.dates <- tweet.dates[-which(names(users)=="~")] #removes the [tweet.dates] values matching those in [users]
      users <- users[-which(names(users)=="~")] #removes all users marked with "~"
    }
    
    while(length(tweet.dates) > length(users)) { #ensures [tweet.dates] isn't longer than [users]
      print("Why is one list longer?")
      tweet.dates <- tweet.dates[-length(tweet.dates)] #I don't realistically ever expect this to happen…but hey, just in case
    }
    
    if(length(unique(users)) >= quantity) { #this part only happens when you're essentially done
      print("Time for a trim?")
      while(length(unique(users)) > quantity) {
        users <- users[-length(users)] #trims the excess at the end so we only get 100 unique user IDs
        tweet.dates <- tweet.dates[-length(tweet.dates)] #the same for the dates
      }
      tweet.dates <- as.POSIXct(tweet.dates,origin="1970-01-01",tz="GMT") #converts the date to be readable
      print("Merging the IDs & dates") # I may need to tweak this slightly?
      for(n in 1:length(users)){ #final formatting
        users[[n]] <-  list(names(users)[[n]],tweet.dates[n],users[[n]]) #upgrades [users] from list of vectors, to list of lists
        names(users[[n]]) <- c("userID","date","followers") #names these sublists
      } #end of the "essentially done" if()
    } else { #this part is for when you don't have enough (unique) user IDs and have to loop through the code again
      print("And again!")
      counter <- (counter + 1) # increases [counter]
      beginning <- (length(users) + 1) #bumps up the start point for the next iteration to save on unnecessary loops
    }
  }
  return(users)
} # it can take over 2 hours to complete this
# orz
