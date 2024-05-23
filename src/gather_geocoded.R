library(optparse)

library(jsonlite)
library(plyr)


parser <- OptionParser()
parser <- add_option(parser, c("-i", "--in_dir"),
                     help="Directory to files containing geocoded toponyms mentioned in books.")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output file with metrics.")
args <- parse_args(parser)


iterate_jsons <- function(directory){
  jsons <- data.frame()
  n_iter <- length(list.files(path=directory))
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "|")
  iter <- 0
  for(filepath in list.files(path=directory)){
    json <- fromJSON(txt=paste0(directory, "\\", filepath), flatten=TRUE)
    if(is.data.frame(json)){
      json$ID <- gsub(x=filepath, pattern="\\.txt$", replacement="")
      jsons <- rbind.fill(jsons, json)
    }
    iter <- iter + 1
    setTxtProgressBar(pb, iter)
  }
  jsons$boundingbox <- as.character(jsons$boundingbox)
  return(jsons)
}


main <- function(args){
  jsons <- iterate_jsons(directory=args$in_dir)
  write.csv(x=jsons, file=args$out_file, sep=",", dec=".", row.names=FALSE, fileEncoding="UTF-8")
}


main(args=args)
