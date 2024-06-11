library(optparse)

library(dplyr)
library(stringr)


parser <- OptionParser()
parser <- add_option(parser, c("-u", "--unique_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-t", "--toponyms_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-g", "--geocoded_file"),
                     help="Filepath to output table with metrics.")
parser <- add_option(parser, c("-s", "--sample_size"),
                     help="")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output table with metrics.")
args <- parse_args(parser)


sample_toponyms <- function(n_toponyms, unique_toponyms, geocoded_toponyms, sample_size){
  unique_toponyms_enriched <- unique_toponyms |>
    inner_join(geocoded_toponyms, by=c("index"="ID"))
  n_toponyms$lemma <- str_remove(string=n_toponyms$lemma, pattern="\\[")
  n_toponyms_enriched <- unique_toponyms_enriched |>
    inner_join(n_toponyms, by=c("toponym"="lemma"))
  n_toponyms_enriched_filtered <- n_toponyms_enriched |>
    filter(place_rank <= 18)
  set.seed(123456)
  indices <- sample(x=1:nrow(n_toponyms_enriched_filtered), size=sample_size)
  n_toponyms_enriched_filtered[indices, c("index", "id", "toponym", "n", "name", "display_name",
                                          "address.state", "address.region")]
}


main <- function(args){
  unique_toponyms <- read.csv(file=args$unique_file)
  n_toponyms <- read.csv(file=args$toponyms_file)
  geocoded_toponyms <- read.csv(file=args$geocoded_file)
  sample_size <- as.numeric(args$sample_size)
  sampled_toponyms <- sample_toponyms(unique_toponyms=unique_toponyms, n_toponyms=n_toponyms,
                                      geocoded_toponyms=geocoded_toponyms,
                                      sample_size=sample_size)
  write.csv(x=sampled_toponyms[order(sampled_toponyms$id),], file=args$out_file, sep=",",
            row.names=FALSE, fileEncoding="UTF-8")
}


main(args=args)
