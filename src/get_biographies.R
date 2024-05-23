library(optparse)


parser <- OptionParser()
parser <- add_option(parser, c("-i", "--in_file"),
                     help="Filepath to input file.")
parser <- add_option(parser, c("-r", "--home_file"),
                     help="Filepath to output file.")
parser <- add_option(parser, c("-m", "--migration_file"),
                     help="Filepath to output file.")
args <- parse_args(parser)


main <- function(args){
  sample_metadata <- read.csv(file=args$in_file)
  authors_homes <- as.data.frame(unique(sample_metadata[, c("author")]))
  authors_homes$home_region <- ""
  colnames(authors_homes)[1] <- "author"
  authors_migrations <- as.data.frame(unique(sample_metadata[, c("author")]))
  colnames(authors_migrations)[1] <- "author"
  authors_migrations$year <- ""
  authors_migrations$migration <- ""
  write.csv(x=authors_homes[order(authors_homes$author), ], file=args$home_file,
            row.names=FALSE, fileEncoding="UTF-8")
  write.csv(x=authors_migrations[order(authors_migrations$author), ],
            file=args$migration_file, row.names=FALSE, fileEncoding="UTF-8")
}


main(args)
