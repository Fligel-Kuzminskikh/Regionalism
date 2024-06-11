library(optparse)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)


parser <- OptionParser()
parser <- add_option(parser, c("-m", "--meta_sample"),
                     help="Filepath to metadata of books in sample.")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-t", "--toponyms_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-q", "--queries_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-u", "--udpipe_model_file"),
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
    temp <- data.frame()
    entities_toponyms <- data.frame()
    tokens <- data.frame()
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


process_toponyms <- function(unique_toponyms, udpipe_model_filepath){
  ud <- udpipe_load_model(file=udpipe_model_filepath)
  
  x <- udpipe_annotate(ud, x=unique_toponyms[unique_toponyms$more_one == TRUE, "toponym"])
  x <- as.data.frame(x)
  
  filtered <- x |>
    filter(!x$token %in% c("весь", "наш", "родной", "далекий", "маленький", "страна", "ваш",
                           "славный", "государство", "город", "самый"))
  filtered$gender <- str_extract_all(string=filtered$feats,
                                     pattern="Gender\\=Masc|Gender\\=Fem|Gender\\=Neut",
                                     simplify=TRUE)[,1]
  filtered$gender <- str_remove_all(string=filtered$gender, pattern="Gender\\=")
  indices <- unique(
    filtered[which((filtered$xpos == "ADJ") | (filtered$upos == "ADJ")), "doc_id"])
  for(index in indices){
    genders <- unique(filtered[filtered$doc_id == index, "gender"])
    genders <- genders[!genders %in% c(NA, "")]
    if((length(genders) > 1) & ("Fem" %in% genders)){
      inds <- filtered[which((filtered$doc_id == index) & ((filtered$xpos == "ADJ") | (filtered$upos == "ADJ"))), "token_id"]
      is <- unique(filtered[which((filtered$doc_id == index) & ((filtered$xpos == "ADJ") | (filtered$upos == "ADJ"))), "sentence_id"])
      for(i in is){
        for(ind in inds){
          head <- filtered[which((filtered$sentence_id == i) & (filtered$doc_id == index) & (filtered$token_id == ind)), "head_token_id"]
          gender <- filtered[which((filtered$sentence_id == i) & (filtered$doc_id == index) & (filtered$token_id == head)), "gender"]
          if(length(gender) > 0){
            if(is.na(gender) == FALSE){
              if(gender == "Fem"){
                token <- filtered[which((filtered$sentence_id == i) & (filtered$doc_id == index) & (filtered$token_id == ind)), "token"]
                if(str_detect(string=token, pattern="(?<![чк])ий$")){
                  filtered[which((filtered$sentence_id == i) & (filtered$doc_id == index) & (filtered$token_id == ind)), "token"] <- str_replace(string=token, pattern="(?<![чк])ий$", replacement="яя")
                }
                if(str_detect(string=token, pattern="(?<=[чк])ий$")){
                  filtered[which((filtered$sentence_id == i) & (filtered$doc_id == index) & (filtered$token_id == ind)), "token"] <- str_replace(string=token, pattern="(?<=[чк])ий$", replacement="ая")
                }
                if(str_detect(string=token, pattern="ый$")){
                  filtered[which((filtered$sentence_id == i) & (filtered$doc_id == index) & (filtered$token_id == ind)), "token"] <- str_replace(string=token, pattern="ый$", replacement="ая")
                }
              }
            }
          }
        }
      }
    }
  }
  filtered[which(filtered$token == "птичая"), "token"] <- "птичья"
  filtered[which(filtered$token == "щучья"), "token"] <- "щучья"
  filtered[which(filtered$token == "ребячая"), "token"] <- "ребячья"
  return(filtered)
}


main <- function(args){
  sample_metadata <- read.csv(file=args$meta_sample)
  n_toponyms <- iterate_books(sample_metadata=sample_metadata)
  unique_toponyms <- data.frame(toponym=unique(n_toponyms$lemma))
  query_part_1 <- 'curl "https://nominatim.openstreetmap.org/search?q='
  query_part_3 <- '&format=json&addressdetails=1&limit=1&extratags=1'
  query_part_4 <- '&email=romanlisyukov@gmail.com'
  query_part_5 <- '&accept-language=ru-RU"'
  unique_toponyms$more_one <- (stri_count_words(unique_toponyms$toponym) > 1)
  unique_toponyms <- process_toponyms(unique_toponyms=unique_toponyms,
                                      udpipe_model_filepath=args$udpipe_model_file)
  # unique_toponyms$index <- as.character(paste0("E", 1:nrow(unique_toponyms)))
  geocoding_queries <- paste0(query_part_1,
                              URLencode(gsub(x=unique_toponyms$toponym, pattern="\\[",
                                             replacement="")), query_part_3, query_part_4,
                              query_part_5,
                    paste0(" -o ../data/json/'", unique_toponyms$index, "'.txt\nsleep 1"))
  writeLines(text=geocoding_queries, con=args$queries_file)
  write.csv(x=unique_toponyms, file=args$toponyms_file, col.names=TRUE, row.names=FALSE,
             sep=",", fileEncoding="UTF-8")
  write.csv(x=n_toponyms, file=args$out_file, col.names=TRUE, row.names=FALSE,
            sep=",", fileEncoding="UTF-8")
}


main(args=args)
