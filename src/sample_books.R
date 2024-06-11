library(optparse)

library(dplyr)
library(stringr)
library(readr)
library(data.table)
library(ggplot2)


options(scipen=999999)


parser <- OptionParser()

parser <- add_option(parser, c("-i", "--in_file"),
                     help="Filepath to metadata.csv that contains books' metadata.")

parser <- add_option(parser, c("-l", "--min_year_birth"),
                     help="Minimum year of author's birth.")

parser <- add_option(parser, c("-u", "--max_year_birth"),
                     help="Maximum year of author's birth.")

parser <- add_option(parser, c("-t", "--tok_dir"),
                     help="Filepath to directory that contains .tokens files.")

parser <- add_option(parser, c("-p", "--out_plot"),
                     help="Filepath to plot of distribution of books' length.")

parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to metadata of books in sample.")

args <- parse_args(parser)


get_tokens_metadata <- function(books_metadata, min_year_birth, max_year_birth, tokens_directory){
  books_metadata <- books_metadata %>%
    filter((author_birth_year >= as.numeric(args$min_year_birth)) & (
      author_birth_year <= as.numeric(args$max_year_birth)))
  
  books_metadata$tokens_filename <- lapply(X=books_metadata$filename, FUN=gsub,
                                     pattern="\\.vert$", replacement=".tokens",
                                     perl=TRUE)
  books_metadata$tokens_filename <- lapply(X=books_metadata$tokens_filename, FUN=gsub,
                                     pattern="^\\d{4}s\\/", replacement="",
                                     perl=TRUE)
  books_metadata$tokens_directory <- lapply(X=books_metadata$filename, FUN=gsub,
                                      pattern="\\.vert$", replacement="",
                                      perl=TRUE)
  books_metadata$tokens_filepath <- paste0(tokens_directory, "/",
                                     books_metadata$tokens_directory, "/",
                                     books_metadata$tokens_filename)
  return(books_metadata)
}
  

count_n_tokens <- function(books_metadata){
  n_iter <- nrow(books_metadata)
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "|")
  iter <- 0
  for(i in 1:nrow(books_metadata)){
    tryCatch({books_metadata[i, 'n_tokens'] <- nrow(read.delim(
      books_metadata[i, 'tokens_filepath'], quote=""))[1]},
      error=function(e){print(paste0(books_metadata[i, 'tokens_filepath'],
                                     " cannot be read!"))})
    iter <- iter + 1
    setTxtProgressBar(pb, iter)
  }
  return(books_metadata)
}


visualize_n_tokens <- function(books_metadata, output_plot){
  books_metadata %>%
    ggplot(aes(x=n_tokens)) +
    geom_histogram(fill="#999999", color="#000000", alpha=0.5) +
    theme_minimal() +
    labs(x="Количество токенов", y="Количество книг") +
    xlim(0, 250000) +
    theme(text=element_text(family="serif")) +
    theme(text=element_text(size=16))
  ggsave(output_plot, width=8, height=6)
}


main <- function(args){
  books_metadata <- read.csv(file=args$in_file)
  books_metadata <- get_tokens_metadata(books_metadata=books_metadata,
                                        min_year_birth=args$min_year_birth,
                                        max_year_birth=args$max_year_birth,
                                        tokens_directory=args$tok_dir)
  books_metadata <- count_n_tokens(books_metadata=books_metadata)
  visualize_n_tokens(books_metadata=books_metadata, output_plot=args$out_plot)
  fwrite(books_metadata, file=args$out_file)
}

main(args)
