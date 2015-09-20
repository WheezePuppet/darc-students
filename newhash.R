library(parallel)

## get all the filenames for August
aug <- list.files("/data/twittersensor/streams2/US_Streaming/2015/Aug",
                  pattern="csv",recursive=TRUE,full.names=TRUE)
## get all the filenames for September
sep <- list.files("/data/twittersensor/streams2/US_Streaming/2015/Sep",
                  pattern="csv",recursive=TRUE,full.names=TRUE)


## Find all the unique hash tags in August.
## This is parallelized to use 4 cores for speed.
## Change the number of cores to fit your machine. Hint: always set it to
## less than the total number of cores you have, and watch the memory usage
## with 'top' in a terminal or a system monitor. If the processes start using
## close to total_memory/mc.cores each you are about to lock up your machine,
## so you should kill the processes (control C) and reduce mc.cores.
## To run on one core, either change mc.cores to 1, (which I haven't tested)
## or change mclapply to lapply and drop the mc.cores argument.
aug.hashes <- unique(unlist(mclapply(aug,
   function(file){
      if(file.info(file)$size>200){
         tweets <- read.csv(file,stringsAsFactors=FALSE)
         ## Hashtag defn:
         ## Hash character '#' followed by:
         ## at least one letter
         ## any number of letters, numbers and the '_'
         ## word boundary
         z <- gregexpr("#[[:alpha:]](_|[[:alnum:]])*\\b",tweets$title)
         ## strip out exactly the hash tags and keep only one copy of each.
         hashes <- unique(unlist(regmatches(tweets$title,z)))
         ## print out some stats as it runs. Note that the files likely
         ## will print out of order because we are using multiple cores.
         cat(basename(file),length(hashes),"\n")
         hashes
      }
   },mc.cores=4)))

## Find all the unique hash tags in September.
## Note that we don't "unique" this (yet).
## We want to count the number of times each hash appears.
sep.hashes <- unlist(mclapply(sep,
   function(file){
      if(file.info(file)$size>200){
         tweets <- read.csv(file,stringsAsFactors=FALSE)
         z <- gregexpr("#[[:alpha:]](_|[[:alnum:]])*\\b",tweets$title)
         ## Note that we don't "unique" these.
         ## We want to count the number of times each hash appears.
         ## Deja vu all over again.
         hashes <- unlist(regmatches(tweets$title,z))
         cat(basename(file),length(hashes),"\n")
         hashes
      }
   },mc.cores=4))

## Now count the number of times each hash appears,
sep.hashes.counts <- table(sep.hashes)
## and then get the unique hashtags.
sep.hashes <- names(sep.hashes.counts)
## Note: we could just as easily done unique(sep.hashes) but this
## is a tad more efficient, since the names of the count table have
## already been "unique"ed.

## New hashtags are those appearing in September but not in August.
new.hashes <- setdiff(sep.hashes,aug.hashes)

## Now order the new hashtags by number of times they appear in September,
## in decreasing order:
z <- which(names(sep.hashes.counts) %in% new.hashes)
new.hashes.counts <- sep.hashes.counts[z]
nh <- sort(new.hashes.counts,decreasing=TRUE)

## There are 881,064 new hash tags (as of 5 am Sept 15) of 1,387,578
## hashtags (so far) in Sept, 2,490,709 hashtags in Aug.

## -- cut here --
## Look at the top 20 most frequent new hashtags for September:
#nh[1:20]
#          #pcmMAJORS16           #fcaMAJORS16        #NeverForget911 
#                 79416                  46620                   9710 
#   #FollowMeDolanTwins     #ALDUBTheAbduction  #ALDUBBATTLEForACause 
#                  8838                   6743                   5803 
#          #pcmMajors16    #followmedolantwins                #MTVEMA 
#                  3688                   3267                   2539 
#    #ALDUBForTALKNTEXT             #DallasDMs       #AskNashAndHayes 
#                  2511                   1999                   1966 
#            #LSUGameOn          #Q102LovesBea    #HappyBirthdayNiall 
#                  1918                   1829                   1792 
##ALDUBWorthFightingFor        #RuinABoardGame             #ALDUBKoTo 
#                  1750                   1724                   1718 
#
#
### hashtags appearing more than 1000 times in the first half of Sept.
#nh[nh>=1000]
#sep.hashes
#           #pcmMAJORS16            #fcaMAJORS16         #NeverForget911 
#                  79416                   46620                    9710 
#    #FollowMeDolanTwins      #ALDUBTheAbduction   #ALDUBBATTLEForACause 
#                   8838                    6743                    5803 
#           #pcmMajors16     #followmedolantwins                 #MTVEMA 
#                   3688                    3267                    2539 
#     #ALDUBForTALKNTEXT              #DallasDMs        #AskNashAndHayes 
#                   2511                    1999                    1966 
#             #LSUGameOn           #Q102LovesBea     #HappyBirthdayNiall 
#                   1918                    1829                    1792 
# #ALDUBWorthFightingFor         #RuinABoardGame              #ALDUBKoTo 
#                   1750                    1724                    1718 
#     #VoteForTeamSlayes          #FollowMeHayes        #ALDUBWishIMaine 
#                   1603                    1559                    1549 
#           #Remember911       #ALDUBSeeYouAgain #MarriageAdviceIn3Words 
#                   1496                    1465                    1439 
#        #ALDUBSaveMaine       #ALDUB7thWEEKSARY             #pcmMAJOR16 
#                   1336                    1325                    1315 
#         #FollowMeSebas                 #BeyDay    #ALDUBLostWithoutYou 
#                   1284                    1267                    1256 
#           #pcmmajors16         #ALDUBKapitLang             #UNLVGameOn 
#                   1182                    1178                    1145 
#           #UsOpenFinal  #RejectedAppleProducts        #AnnoyMeIn5Words 
#                   1095                    1040                    1005 
#    #VoteForTeamSlayes         #FollowMeHayes 
#                  1603                   1559 

## 556 hashtags appear at least 100 times
## 82 hashtags appear at least 500 times
## 36 hashtags appear at least 1000 times
