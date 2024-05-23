library(optparse)
library(dplyr)
library(stringr)
library(tidyr)


parser <- OptionParser()
parser <- add_option(parser, c("-m", "--meta_sample"),
                     help="Filepath to metadata of books in sample.")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-t", "--toponyms_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-q", "--queries_file"),
                     help="Filepath to output table with metrics.")
args <- parse_args(parser)


upload_filter_entities <- function(filepath){
  entities <- read.delim(filepath, quote="")
  entities %>%
    filter((cat == "GPE") & (prop == "PROP"))
}

get_lemmas <- function(entities, tokens){
  for(i in 1:nrow(entities)){
    if(entities[i, "start_token"] == entities[i, "end_token"]){
      entities[i, "lemma"] <- tokens[tokens$token_ID_within_document == entities[i, "start_token"],
                         "lemma"]
    }
    else{
      words_ids <- entities[i, "start_token"]:entities[i, "end_token"]
      entities[i, "lemma"] <- paste(tokens[tokens$token_ID_within_document %in% words_ids, "lemma"], collapse=" ")
    }
  }
  return(entities)
}

# upload_sample_metadata <- function(filepath){
  # read.csv(filepath)
# }


count_toponyms <- function(entities_toponyms){
  entities_toponyms %>%
    select(-c(end_token, prop, cat, COREF, start_token, text)) %>%
    count(lemma)
}


iterate_books <- function(sample_metadata){
  perm <- data.frame()
  n_iter <- nrow(sample_metadata)
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "|")
  iter <- 0
  for(i in 1:nrow(sample_metadata)){
    tryCatch({tokens <- read.delim(file=sample_metadata[i, 'tokens_filepath'],
                                   quote="")}, error=function(e){print(paste0(
                                     sample_metadata[i, 'tokens_filepath'],
                                     " cannot be read!"))})
    tryCatch({entities_toponyms <- upload_filter_entities(filepath=gsub(
      x=sample_metadata[i, 'tokens_filepath'], pattern="\\.tokens$",
      replacement=".entities"))},
      error=function(e){print(paste0(sample_metadata[i, 'tokens_filepath'],
                                     " cannot be read!"))})
    if(exists("entities_toponyms")){
      if(nrow(entities_toponyms) > 0){
        entities_toponyms <- get_lemmas(entities=entities_toponyms, tokens=tokens)
        temp <- count_toponyms(entities_toponyms=entities_toponyms)
      }
    }
    if(exists("temp")){
      if(nrow(temp) > 0){
        temp[,'id'] <- sample_metadata[i, 'id']
        perm <- rbind(perm, temp)
        iter <- iter + 1
        setTxtProgressBar(pb, iter)
      }
    }
  }
  return(perm)
}


# get_process_unique_toponyms <- function(n_toponyms){
  # unique_toponyms <- 
  # data.frame(toponym=unique(n_toponyms$text))
  # unique_toponyms$lemma_s <- system2("mystem", c("-d", "-l", "-c"),
    #                                 input = unique_toponyms$text,
     #                                stdout = TRUE)
  # return(unique_toponyms)
# }


main <- function(args){
  sample_metadata <- read.csv(file=args$meta_sample) # upload_sample_metadata(args$meta_sample)
  n_toponyms <- iterate_books(sample_metadata=sample_metadata)
  unique_toponyms <- data.frame(toponym=unique(n_toponyms$lemma)) # get_process_unique_toponyms(n_toponyms=n_toponyms)
  query_part_1 <- 'curl "https://nominatim.openstreetmap.org/search?q='
  query_part_3 <- '&format=json&addressdetails=1&limit=1&extratags=1'
  query_part_4 <- '&limit=1&email=romanlisyukov@gmail.com'
  query_part_5 <- '&accept-language=ru-RU"'
  unique_toponyms$index <- as.character(paste0("E", 1:nrow(unique_toponyms)))
  # toponyms <- unique_toponyms$toponym
  # toponyms <- gsub(pattern="\\'", replacement="", x=toponyms)
  # toponyms <- gsub(pattern='\\"', replacement='', x=toponyms)
  # toponyms <- gsub(pattern='\\`', replacement='', x=toponyms)
  geocoding_queries <- paste0(query_part_1, URLencode(unique_toponyms$toponym),
                                    query_part_3, query_part_4, query_part_5,
                    paste0(" -o ../data/json/'", unique_toponyms$index, "'.txt\nsleep 1"))
  writeLines(text=geocoding_queries, con=args$queries_file)
  write.csv(x=unique_toponyms, file=args$toponyms_file, col.names=TRUE, row.names=FALSE,
             sep=",", fileEncoding="UTF-8")
  write.csv(x=n_toponyms, file=args$out_file, col.names=TRUE, row.names=FALSE,
            sep=",", fileEncoding="UTF-8")
}


main(args)
