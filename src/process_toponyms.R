library(optparse)

library(stringi)


parser <- OptionParser()
parser <- add_option(parser, c("-i", "--in_file"),
                     help="Filepath to file containing unique toponyms mentioned in books.")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output file with metrics.")
args <- parse_args(parser)


main <- function(args){
  unique_toponyms <- read.csv(args$in_file)
  unique_toponyms$more_one <- (stri_count_words(unique_toponyms$toponym) > 1)
  write.csv(x=unique_toponyms, file=args$out_file, col.names=TRUE, row.names=FALSE,
            sep=",", fileEncoding="UTF-8")
}


main(args=args)
