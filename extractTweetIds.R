
library(httr)
library(stringr)

source("twitterLogonInfo.R")


liv <- content(GET("http://otter.topsy.com/search.js?callback=jQuery183007628459153908795_1444843841679&q=%23BlackLivesMatter&type=tweet&offset=0&perpage=10&window=h14&sort_method=-date&call_timestamp=1444843852435&apikey=09C43A9B270A470B8EB8F2946A9369F3&_=1444843853148"),as="text")

tweet.urls <- 
    str_extract_all(liv, "http://twitter.com/[^/]*/status/[0-9]*")[[1]]

