library(optparse)

# library(rdadata)
library(plyr)
library(jsonlite)


parser <- OptionParser()
parser <- add_option(parser, c("-i", "--in_file"),
                     help="Filepath to file containing unique toponyms mentioned in books.")
# parser <- add_option(parser, c("-a", "--api_token"),
  #                   help="API token.")
# parser <- add_option(parser, c("-s", "--secret_token"),
#                      help="Secret token.")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output file with metrics.")
args <- parse_args(parser)


# load_file_get_unique_toponyms <- function(filepath){
  # n_toponyms <- read.csv(file=filepath)
  # unique(n_toponyms$text)
# }


iterate_toponyms <- function(unique_toponyms){
  perm <- data.frame()
  n_iter <- nrow(unique_toponyms)
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "|")
  iter <- 0
  for(unique_toponym in unique_toponyms$toponym){
    query_part_1 <- 'curl "https://nominatim.openstreetmap.org/search?q='
    query_part_2 <- URLencode(unique_toponym)
    query_part_3 <- '&format=json&addressdetails=1&limit=1&extratags=1'
    query_part_4 <- '&limit=1&email=romanlisyukov@gmail.com'
    query_part_5 <- '&accept-language=ru-RU"'
    query <- paste0(query_part_1, query_part_2, query_part_3, query_part_4,
                    query_part_5)
    temp <- system(command=query, intern=TRUE)[4]
    cat(temp)
    tryCatch({temp <- fromJSON(temp, flatten=TRUE)},
             error=function(e){print(paste0(unique_toponym, " cannot be found!")
                                     )})
    if(is.data.frame(temp) == FALSE){
      temp <- data.frame()
    }
    else{
      temp$TOPONYM <- unique_toponym
    }
    perm <- rbind.fill(perm, temp)
    iter <- iter + 1
    setTxtProgressBar(pb, iter)
    Sys.sleep(time=1)
  }
  return(perm)
}


main <- function(args){
  # save_dadata_tokens(api_token=args$api_token, secret_token=args$secret_token)
  # unique_toponyms <- load_file_get_unique_toponyms(filepath=args$in_file)
  unique_toponyms <- read.csv(file=args$in_file)
  geocoded_toponyms <- iterate_toponyms(unique_toponyms=unique_toponyms)
  write.csv(x=geocoded_toponyms, file=args$out_file, row.names=FALSE,
            fileEncoding="UTF-8")
}


main(args=args)
