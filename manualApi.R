library(RCurl)
library(httr)
library(jsonlite)

source("twitterLogonInfo.R")

auth <- paste("Basic",RCurl::base64(paste(key,secret,sep=":")))
auth.req <- POST("https://api.twitter.com/oauth2/token",
    add_headers(
        "Authorization"=auth,
        "Content-Type"="application/x-www-form-urlencoded;charset=UTF-8"),
    body="grant_type=client_credentials")

access.token <- paste("Bearer",content(auth.req)$access_token)

make.manual.twitter.api.call <- function(the.api.request) {
    return(
        fromJSON(
            content(
                GET(the.api.request,add_headers(Authorization=access.token)),
                as="text")
        )
    )
}

#For example:
#hannahs.info <- make.manual.twitter.api.call("https://api.twitter.com/1.1/users/lookup.json?screen_name=hzontine")
#cat(hannahs.info$description,"\n")
