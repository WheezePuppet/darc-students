#From Liv:

library(httr)
library(stringi)
library(stringr)
library(curl)
library(twitteR)
setup_twitter_oauth(key,secret,access_token,access_token_secret)

#This first one will give you the (repeated) first quantity user IDs. Just unique() it if you only want the unrepeated IDs.
#Make sure to remember to add the ‘#’ symbol in hashtag if you’re searching for a hashtag and not just a word.

get.first.user.ids <- function(hashtag, quantity) {
  hashtag <- ifelse(grepl("#",hashtag),paste0("23",substring(hashtag,2)),hashtag)
  count <- 0
  users <- c()
  unique.users <- 0
  while(length(unique(users)) < quantity) {
    results <- content(GET(paste0("otter.topsy.com/search.js?callback=jquery183007628459153908795_1444843841679&q=%",hashtag,"%20-rt&type=tweet&perpage=",quantity,"&window=a&sort_method=-date&offset=",count*quantity,"&call_timestamp=1444843852435&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148/")),as="text")
    users <- c(users, stri_extract_last_words(str_extract_all(results, 'topsy.com/twitter/[^/]*,')[[1]]))
    
    if(unique.users == length(unique(users))) {
      break()
    }
    
    unique.users <- length(unique(users))
    if(unique.users > quantity) {
      while(length(unique(users)) > quantity) {
        users <- users[-length(users)]
      }
    } else {
      count <- (count + 1)
    }
  }
  for(i in 1:length(users)) {
    if(grepl("[[:alpha:]]",users[i])) {
      users[c(grepl(users[i],users))] <- convert.user(users[i])
    }
  }
  return(users)
}

#In the get.first.user.ids, I call convert.user, which is this next function:

convert.user <- function(user) {
  if(suppressWarnings(is.na(as.numeric(user)))) {
    conversion <- getUser(user)$id
  } else {
    conversion <- getUser(user)$screenName
  }
  return(as.character(conversion))
}

#And here is the last one:

get.user.info <- function(user) {
  user.name <- getUser(user)
  user.id <- user.name$id
  follower.list <- user.name$getFollowerIDs()
  friend.list <- c(user.name$getFriendIDs())
  user.info <- list(user.name,user.id,follower.list,friend.list)
  names(user.info) <- c("username","userID","followerIDs","friendIDs")
  return(user.info)
}
