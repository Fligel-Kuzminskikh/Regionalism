library(optparse)

library(dplyr)
library(tidyr)


parser <- OptionParser()
parser <- add_option(parser, c("-g", "--geocoded_file"),
                     help="Filepath to file containing geocoded toponyms mentioned in books.")
parser <- add_option(parser, c("-s", "--sample_metadata_file"),
                     help="Filepath to file containing metadata of books and authors.")
parser <- add_option(parser, c("-a", "--authors_homes_file"),
                     help="Filepath to file containing data on home regions of authors.")
parser <- add_option(parser, c("-m", "--authors_migrations_file"),
                     help="Filepath to file containing migration statuses of authors.")
parser <- add_option(parser, c("-u", "--unique_toponyms_file"),
                     help="Filepath to file containing unique toponyms.")
parser <- add_option(parser, c("-n", "--n_toponyms_file"),
                     help="Filepath to file containing counts of toponyms.")
parser <- add_option(parser, c("-l", "--level"),
                     help="Level of operationalization of home region.")
parser <- add_option(parser, c("-o", "--out_file"),
                     help="Filepath to output file containing variables for stat. modelling.")
args <- parse_args(parser)


upload_process_geocoded <- function(geocoded_file){
  
  geocoded_toponyms <- read.csv(file=geocoded_file)
  
  geocoded_toponyms <- geocoded_toponyms |>
    filter(geocoded_toponyms$place_rank <= 18)
  
  for(state in c("Республика Крым", "Севастополь")){
    geocoded_toponyms[which(geocoded_toponyms$address.state == state), "address.region"] <- state
  }
  
  geocoded_toponyms[which(geocoded_toponyms$address.state %in% c("Республика Крым", "Севастополь")), "address.country"] <- "Украина"
  
  geocoded_toponyms[which(geocoded_toponyms$address.region == "Сибирь"), "address.region"] <- "Сибирский федеральный округ"
  for(name in c("Баку", "Севастополь", "Киев", "Минск", "Ташкент", "Кишинёв",
                "Кагульский район")){
    geocoded_toponyms[which(geocoded_toponyms$name == name), "address.region"] <- name
    
    geocoded_toponyms[which(geocoded_toponyms$name == name), "address.state"] <- name
  }
  return(geocoded_toponyms)
}


process_migrations <- function(sample_metadata, authors_migrations, level){
  sample_metadata[sample_metadata$author_death_year == "", "author_death_year"] <- "2024"
  sample_metadata$author_death_year <- as.numeric(
    as.character(sample_metadata$author_death_year))
  sample_metadata$author_birth_year <- as.numeric(
    as.character(sample_metadata$author_birth_year))
  sample_metadata[sample_metadata$author == "Козлов, Вильям Федорович", "author_death_year"] <- 2009
  perm <- data.frame()
  authors_years <- unique(sample_metadata[, c("author", "author_birth_year", "author_death_year")])
  for(author in authors_years$author){
    birth <- authors_years[authors_years$author == author, "author_birth_year"]
    death <- authors_years[authors_years$author == author, "author_death_year"]
    if(is.na(birth) == FALSE){
      years <- birth:death
    }
    temp <- data.frame(a=author, y=years, m=c("stayed", rep(NA, length(years)-1)))
    perm <- rbind(perm, temp)
  }
  
  if(level == "region"){
    
  authors_migrations <- authors_migrations |>
    mutate(migration_region = if_else(migration_region != "", migration_region,
                                      migration_state))
  
  perm_enriched <- perm |>
    inner_join(na.omit(authors_migrations[,c("author", "year", "migration_region")]),
               by=c("a"="author"))
  
  for(i in 1:nrow(perm_enriched)){
    if(is.na(perm_enriched[i, "year"]) == FALSE){
      if(perm_enriched[i, "y"] == perm_enriched[i, "year"]){
        perm_enriched[i, "m"] <- perm_enriched[i, "migration_region"]
      }
    }
  }
  }
  
  else if(level == "state"){
    
    perm_enriched <- perm |>
      inner_join(na.omit(authors_migrations[,c("author", "year", "migration_state")]),
                 by=c("a"="author"))
    
    for(i in 1:nrow(perm_enriched)){
      if(is.na(perm_enriched[i, "year"]) == FALSE){
        if(perm_enriched[i, "y"] == perm_enriched[i, "year"]){
          perm_enriched[i, "m"] <- perm_enriched[i, "migration_state"]
        }
      }
    }
  }
  
  pre_final <- unique(perm_enriched[, c("a", "y", "m")])
  
  pre_final <- pre_final |>
    fill(m, .direction="down")
  
  pre_final %>%
    mutate(m=if_else(m %in% c("returned", "stayed"), 1, 2))
}


process_toponyms <- function(level, sample_metadata_file, authors_homes_file,
                             authors_migrations_file, n_toponyms_file, geocoded_toponyms,
                             unique_toponyms_file){
  unique_toponyms <- read.csv(file=unique_toponyms_file)
  sample_metadata <- read.csv(file=sample_metadata_file)
  n_toponyms <- read.csv(file=n_toponyms_file)
  authors_homes <- read.csv(file=authors_homes_file)
  authors_migrations <- read.csv(file=authors_migrations_file)
  
  unique_toponyms_enriched <- unique_toponyms %>%
    inner_join(y=geocoded_toponyms[, c("ID", "name", "address.state", "address.region",
                                       "address.country")], by=c("index"="ID"))
  
  unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.state) == TRUE, "address.state"] <- unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.state) == TRUE, "address.country"]
  unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.region) == TRUE, "address.region"] <- unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.region) == TRUE, "address.state"]
  unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.state) == TRUE, "address.state"] <- unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.state) == TRUE, "name"]
  unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.region) == TRUE, "address.region"] <- unique_toponyms_enriched[is.na(unique_toponyms_enriched$address.region) == TRUE, "name"]
  n_toponyms_enriched <- n_toponyms %>%
    inner_join(unique_toponyms_enriched[, c("toponym", "address.state", "address.region")],
               by=c("lemma"="toponym"))
  
  sample_metadata_enriched <- sample_metadata %>%
    inner_join(n_toponyms_enriched) %>%
    select(id, author, n, year, address.state, address.region)
  
  states_regions <- unique(geocoded_toponyms[,c("address.state", "address.region")])
  
  authors_homes_enriched <- authors_homes |>
    left_join(states_regions, by=c("home_state"="address.state"))
  
  colnames(authors_homes_enriched)[4] <- "home_region"
  
  authors_homes_enriched$home_region_status <- 9
  
  authors_homes_enriched[which(authors_homes_enriched$home_region == "Центральный федеральный округ"), "home_region_status"] <- 1
  
  authors_homes_enriched[which(authors_homes_enriched$home_region == "Северо-Западный федеральный округ"), "home_region_status"] <- 2
  
  authors_homes_enriched[which(authors_homes_enriched$home_region %in% c("Киев", "Минск", "Ташкент", "Баку")), "home_region_status"] <- 3
  
  authors_homes_enriched[which((is.na(authors_homes_enriched$home_region) == FALSE) & authors_homes_enriched$home_region_status == 9), "home_region_status"] <- 4
  
  authors_homes_enriched_filtered <- authors_homes_enriched |>
    filter(is.na(authors_homes_enriched$home_region) == FALSE)
  
  a_h <- authors_homes_enriched_filtered[c("author", "home_region_status")]
  
  sample_metadata_enriched <- sample_metadata_enriched %>%
    inner_join(authors_homes_enriched_filtered)
  
  if(level == "state"){
    sample_metadata_enriched <- sample_metadata_enriched |>
      mutate(relation = if_else(
        sample_metadata_enriched$home_state == sample_metadata_enriched$address.state, 2, 1))
  }
  else if(level == "region"){
    sample_metadata_enriched <- sample_metadata_enriched |>
      mutate(relation = if_else(
        sample_metadata_enriched$home_region == sample_metadata_enriched$address.region, 2, 1))
  }
  
  pre_final <- process_migrations(sample_metadata=sample_metadata,
                                      authors_migrations=authors_migrations, level=level)
  
  sample_metadata_enriched <- sample_metadata_enriched |>
    group_by(id, author, year, relation) |>
    summarise(sum = sum(n))
  
  sample_metadata_enriched <- sample_metadata_enriched |>
    inner_join(pre_final, by=c("author"="a", "year"="y"))
  
  sample_metadata_enriched_wide <- sample_metadata_enriched |>
    pivot_wider(names_from=relation, values_from=sum)
  
  sample_metadata_enriched_wide[is.na(sample_metadata_enriched_wide)] <- 0
  
  sample_metadata_enriched_wide$a <- sample_metadata_enriched_wide$`1` + sample_metadata_enriched_wide$`2`
  
  d <- sample_metadata_enriched_wide[,c("id", "author", "a", "m", "2")]
  
  d <- d |>
    inner_join(a_h)
  
  d <- na.omit(d)
  
  colnames(d)[2:6] <- c("i", "a", "m", "h", "s")
  
  d[d$i == "Можейко, Игорь Всеволодович ", "i"] <- "Булычев, Кир"
  
  unique_authors <- unique(d[, c("i")])
  unique_authors$i_2 <- 1:nrow(unique_authors)
  
  d |>
    inner_join(unique_authors)
}


main <- function(args){
  geocoded_toponyms <- upload_process_geocoded(geocoded_file=args$geocoded_file)
  d <- process_toponyms(level=args$level, sample_metadata_file=args$sample_metadata_file,
                        authors_homes_file=args$authors_homes_file,
                        authors_migrations_file=args$authors_migrations_file,
                        n_toponyms_file=args$n_toponyms_file,
                        geocoded_toponyms=geocoded_toponyms,
                        unique_toponyms_file=args$unique_toponyms_file)
  write.csv(x=d, file=args$out_file, col.names=TRUE, row.names=FALSE,
            sep=",", fileEncoding="UTF-8")
}


main(args=args)
