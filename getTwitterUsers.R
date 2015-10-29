#From Liv:

library(httr)
library(stringi)
library(stringr)
library(curl)
library(twitteR)

#be sure to do this part!
setup_twitter_oauth(key,secret,access_token,access_token_secret)

#this first one will give you the (repeated) first quantity user IDs. Just unique() it if you only want the unrepeated IDs.
#make sure to remember to add the ‘#’ symbol in hashtag if you’re searching for a hashtag and not just a word.
get.first.user.ids <- function(hashtag, quantity) {
  hashtag <- ifelse(grepl("#",hashtag),paste0("23",substring(hashtag,2)),hashtag)
  count <- 0 #if you want, you can manually change this if, say, you already have the first 50 and you want the next 50
  users <- c()
  unique.users <- 0 #we want this to match [quantity] in the end
  while(length(unique(users)) < quantity) {
    results <- content(GET(paste0("otter.topsy.com/search.js?callback=jquery183007628459153908795_1444843841679&q=%",hashtag,"%20-rt&type=tweet&perpage=",quantity,"&window=a&sort_method=-date&offset=",count*quantity,"&call_timestamp=1444843852435&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/")),as="text")
    users <- c(users, stri_extract_last_words(str_extract_all(results, 'topsy.com/twitter/[^/]*,')[[1]])) #collects usernames
    
    if(unique.users==length(unique(users))) { 
      break() #breaks if our vector of users doesn't increase
    }
    
    #this part is for all those increadibly annoying people who decided to delete their twitter or set it to protected
    #I'm lookin' at you, @_ninjalove and @kathrinoutloud >:(
    for(i in (unique.users + 1):length(users)) { #only looks at new data
      if(grepl("[[:alpha:]]",users[i])) { #reduces twitteR calls by only grabbing usernames—fails in the case of all-numeral usernames
        if(curl_fetch_memory(paste0("twitter.com/",users[i]))$status_code==404) { #filters out deleted users
          users[c(grepl(users[i],users))] <- NA #replaces all deleted usernames with 'NA'
        } else {
          user.info <- getUser(users[i]) #calls twitteR here so I don't have to do it twice below
          users[c(grepl(users[i],users))] <- ifelse(user.info$protected,NA,user.info$id) #converts matching usernames to user ID/replaces protected usernames with 'NA'
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
  }
  return(users)
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
  user.name <- getUser(user) #uses twitteR to get the info
  user.id <- user.name$id #grabs the user ID out of said info
  follower.list <- user.name$getFollowerIDs() #and the follower IDs
  friend.list <- c(user.name$getFriendIDs()) #you get the picture
  user.info <- list(user.name,user.id,follower.list,friend.list) #a list of lists
  names(user.info) <- c("username","userID","followerIDs","friendIDs") #names lists for your convenience
  return(user.info)
} #the end
