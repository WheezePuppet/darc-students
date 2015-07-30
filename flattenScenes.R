
# in.file is a comma-separated file with (possibly) more than one entry per
# line. out.file will be given all the proper combinations to break it down
# into pairs only.
#
# example:
#  in.file:
#   a,b
#   c,d,e,f
#
#  out.file:
#   a,b
#   c,d
#   c,e
#   c,f
#   d,e
#   d,f
#   e,f
flattenScenes <- function(in.file, out.file=paste0(in.file,".flat")) {

    lines <- strsplit(readLines(in.file),",")
    flattened.df <- do.call("rbind",
        lapply(lines, function(line) {
                loc <- line[length(line)]
                chars <- line[-length(line)]
                if (length(chars) >= 2) {
                    df <- data.frame(t(combn(chars,2)),loc=loc)
                    colnames(df) <- c("char1","char2","loc")
                    return(df)
                } else {
                    return(NULL)
                }
            }
        )
    )
    write.csv(flattened.df, file=out.file, row.names=FALSE)
}

