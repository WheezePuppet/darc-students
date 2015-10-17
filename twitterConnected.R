are.twitter.connected <- function(user.id.1, user.id.2){
  call <- paste("https://api.twitter.com/1.1/friendships/show.json?source_id=",user.id.1,"&target_id=",user.id.2, sep="")
  answer <- make.manual.twitter.api.call(call)
  if (answer$relationship$source$following == TRUE || answer$relationship$target$following == TRUE){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}

are.twitter.double.connected <- function(user.id.1, user.id.2){
  call <- paste("https://api.twitter.com/1.1/friendships/show.json?source_id=",user.id.1,"&target_id=",user.id.2, sep="")
  answer <- make.manual.twitter.api.call(call)
  if (answer$relationship$source$following == TRUE && answer$relationship$target$following == TRUE){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}
