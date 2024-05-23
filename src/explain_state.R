library(rethinking)
library(dplyr)
library(tidyr)
library(ggplot2)


geocoded_toponyms <- read.csv(file="C:\\Users\\User\\regionalism\\data\\geocoded_toponyms.csv")

geocoded_toponyms[which(geocoded_toponyms$address.state == "Республика Крым"), "address.region"] <- "Республика Крым"

geocoded_toponyms[which(geocoded_toponyms$address.state == "Республика Крым"), "address.country"] <- "Украина"

geocoded_toponyms[which(geocoded_toponyms$address.state == "Севастополь"), "address.region"] <- "Севастополь"

geocoded_toponyms[which(geocoded_toponyms$address.state == "Севастополь"), "address.country"] <- "Украина"

geocoded_toponyms[which(geocoded_toponyms$address.region == "Сибирь"), "address.region"] <- "Сибирский федеральный округ"

geocoded_toponyms[which(geocoded_toponyms$name == "Баку"), "address.region"] <- "Баку"

geocoded_toponyms[which(geocoded_toponyms$name == "Баку"), "address.state"] <- "Баку"

geocoded_toponyms[which(geocoded_toponyms$name == "Киев"), "address.region"] <- "Киев"

geocoded_toponyms[which(geocoded_toponyms$name == "Киев"), "address.state"] <- "Киев"

geocoded_toponyms[which(geocoded_toponyms$name == "Минск"), "address.region"] <- "Минск"

geocoded_toponyms[which(geocoded_toponyms$name == "Минск"), "address.state"] <- "Минск"

geocoded_toponyms[which(geocoded_toponyms$name == "Ташкент"), "address.region"] <- "Ташкент"

geocoded_toponyms[which(geocoded_toponyms$name == "Ташкент"), "address.state"] <- "Ташкент"

authors_homes <- read.csv(file="C:\\Users\\User\\regionalism\\data\\authors_homes.csv")

authors_migrations <- read.csv(file="C:\\Users\\User\\regionalism\\data\\authors_migrations.csv")

n_toponyms <- read.csv(file="C:\\Users\\User\\regionalism\\data\\n_toponyms.csv")

sample_metadata <- read.csv(file="C:\\Users\\User\\regionalism\\data\\sample_metadata.csv")

unique_toponyms <- read.csv(file="C:\\Users\\User\\regionalism\\data\\unique_toponyms.csv")

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

states_regions <- unique(n_toponyms_enriched[,c("address.state", "address.region")])

sample_metadata_enriched <- sample_metadata %>%
  inner_join(n_toponyms_enriched) %>%
  select(id, author, n, year, address.state, address.region)

authors_homes_enriched <- authors_homes |>
  left_join(states_regions, by=c("home_state"="address.state"))

colnames(authors_homes_enriched)[4] <- "home_region"

authors_homes_enriched$home_region_status <- 9

authors_homes_enriched[which(authors_homes_enriched$home_region == "Центральный федеральный округ"), "home_region_status"] <- 1

authors_homes_enriched[which(authors_homes_enriched$home_region == "Северо-Западный федеральный округ"), "home_region_status"] <- 2

authors_homes_enriched[which(authors_homes_enriched$home_region %in% c("Киев", "Минск", "Ташкент", "Баку")), "home_region_status"] <- 3

authors_homes_enriched[which((is.na(authors_homes_enriched$home_region) == FALSE) & authors_homes_enriched$home_region_status == 9), "home_region_status"] <- 4

authors_homes_enriched_filtered <- authors_homes_enriched |>
  filter(!authors_homes_enriched$home_state %in% c("", "кочует"))

sample_metadata_enriched <- sample_metadata_enriched %>%
  inner_join(authors_homes_enriched_filtered)

sample_metadata_enriched <- sample_metadata_enriched |>
  mutate(relation_state = if_else(sample_metadata_enriched$home_state == sample_metadata_enriched$address.state, 2, 1),
         relation_region = if_else(sample_metadata_enriched$home_region == sample_metadata_enriched$address.region, 2, 1))

sample_metadata[sample_metadata$author_death_year == "", "author_death_year"] <- "2024"
sample_metadata$author_death_year <- as.numeric(as.character(sample_metadata$author_death_year))
sample_metadata$author_birth_year <- as.numeric(as.character(sample_metadata$author_birth_year))
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

perm_enriched <- perm |>
  inner_join(na.omit(authors_migrations), by=c("a"="author"))

for(i in 1:nrow(perm_enriched)){
  if(is.na(perm_enriched[i, "year"]) == FALSE){
    if(perm_enriched[i, "y"] == perm_enriched[i, "year"]){
      perm_enriched[i, "m"] <- perm_enriched[i, "migration"]
    }
  }
}

pre_final <- unique(perm_enriched[, c("a", "y", "m")])

pre_final <- pre_final |>
  fill(m, .direction="down")

sample_metadata_enriched <- sample_metadata_enriched |>
  group_by(id, author, year, relation_state) |>
  summarise(sum = sum(n))

pre_final <- pre_final %>%
  mutate(m=if_else(m %in% c("returned", "stayed"), 1, 2))

sample_metadata_enriched <- sample_metadata_enriched |>
  inner_join(pre_final, by=c("author"="a", "year"="y"))

sample_metadata_enriched_wide <- sample_metadata_enriched |>
  pivot_wider(names_from=relation_state, values_from=sum)

sample_metadata_enriched_wide[is.na(sample_metadata_enriched_wide)] <- 0

sample_metadata_enriched_wide$a <- sample_metadata_enriched_wide$`1` + sample_metadata_enriched_wide$`2`

d <- sample_metadata_enriched_wide[,c("id", "author", "a", "m", "2")]

a_h <- read.csv(file="C:\\Users\\User\\regionalism\\data\\authors_homes.csv")

d <- d |>
  inner_join(a_h[c("author", "home_state_status")])

d <- na.omit(d)

colnames(d)[2:6] <- c("i", "a", "m", "h", "s")

d[d$i == "Можейко, Игорь Всеволодович ", "i"] <- "Булычев, Кир"

unique_authors <- unique(d[, c("i")])
unique_authors$i_2 <- 1:nrow(unique_authors)

d <- d |>
  inner_join(unique_authors)

d_list <- list(
  i_2 = d$i_2,
  h = d$h,
  m = d$m,
  a = d$a,
  s = d$s
)

set.seed(123456)
N <- 100 
aI <- rnorm(N, 0, 1)
aM <- rnorm(N, 0, 1)
aS <- rnorm(N, 0, 1)

prior_predictive_simulation <- data.frame(interceptI=aI, interceptM=aM, interceptS=aS)

prior_predictive_simulation$logit_p <- rowSums(prior_predictive_simulation)

prior_predictive_simulation$p <- inv_logit(prior_predictive_simulation$logit_p)

hist(prior_predictive_simulation$p)

# prior_predictive_simulation <- prior_predictive_simulation |>
  # mutate(slope_direction = if_else(slope > 0, "blue", "red"))

# ggplot(data=d_1, aes(x=st_sum_n_words, y=st_entropy)) +
  # geom_abline(intercept=prior_predictive_simulation$intercept,
    #          slope=prior_predictive_simulation$slope,
     #         color=prior_predictive_simulation$slope_direction,
      #        alpha=0.5) +
  # theme_minimal()

set.seed(123456)
m <- ulam(
  alist(h ~ dbinom(a, p),
        logit(p) <- aI[i_2] + aM[m] + aS[s],
        aI[i_2] ~ dnorm(0, 1),
        aM[m] ~ dnorm(0, 1),
        aS[s] ~ dnorm(0, 1)),
  data=d_list, chains=4, cores=4, iter=2000,
  log_lik=TRUE)
precis(m, depth=2)
set.seed(123456)
post <- extract.samples(m)
# diff_aM <- post$aM[,1] - post$aM[,2]
# diff_p <- inv_logit(post$aM[,1]) - inv_logit(post$aM[,2])
# precis(list(diff_aM=diff_aM, diff_p=diff_p))

model_2_alphas <- as.data.frame(post$aM)

model_2_alphas$inv_V1 <- inv_logit(model_2_alphas$V1)
model_2_alphas$inv_V2 <- inv_logit(model_2_alphas$V2)

colnames(model_2_alphas)[3:4] <- c("ВРодномРегионе", "ВНеродномРегионе")

model_2_alphas_wide <- model_2_alphas |>
  pivot_longer(values_to = "inv_logit", cols=c("ВРодномРегионе", "ВНеродномРегионе"))



model_2_alphas_wide |>
  ggplot( aes(x=inv_logit, fill=name)) +
  geom_histogram(color="#000000", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#0072B2", "#FC4E68"),
                    labels=c("ВНеродномРегионе", "ВРодномРегионе")) +
  theme_minimal() +
  labs(x="Вероятность употребить топоним родного региона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16),
        legend.text = element_text(size=12)) +
  theme(legend.position = c(1, 1.0625), legend.justification = c("right", "top")) +
  guides(fill=guide_legend(title = "")) +
  facet_wrap(~name)
ggsave("plot6.jpg", height=6, width=8)

model_2_alphas$diff <- inv_logit(model_2_alphas$V1) - inv_logit(model_2_alphas$V2)

ggplot(data=model_2_alphas, aes(x=diff)) +
  geom_histogram(fill="#999999", color="#000000", alpha=0.5) +
  theme_minimal() +
  geom_vline(xintercept=mean(model_2_alphas$diff), linetype="dashed", linewidth=3.5,
             color="#000000") +
  geom_vline(xintercept=HPDI(model_2_alphas$diff)[1], linetype="dashed", linewidth=2.5,
             color="#000000") +
  geom_vline(xintercept=HPDI(model_2_alphas$diff)[2], linetype="dashed", linewidth=2.5,
             color="#000000") +
  labs(x="Разность вероятностей употребления топонима родного\nрегиона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16))
ggsave("plot4.jpg", height=6, width=8)

set.seed(123456)
N <- 100
aM <- rnorm(N, 0, 1.666)
aS <- rnorm(N, 0, 1.666)

prior_predictive_simulation <- data.frame(interceptM=aM, interceptS=aS)

prior_predictive_simulation$logit_p <- rowSums(prior_predictive_simulation)

prior_predictive_simulation$p <- inv_logit(prior_predictive_simulation$logit_p)

hist(prior_predictive_simulation$p)

set.seed(123456)
m_2 <- ulam(
  alist(h ~ dbinom(a, p),
        logit(p) <- aM[m] + aS[s],
        aM[m] ~ dnorm(0, 1.666),
        aS[s] ~ dnorm(0, 1.666)),
  data=d_list, chains=4, cores=4, iter=2000,
  log_lik=TRUE)
precis(m_2, depth=2)
set.seed(123456)
post <- extract.samples(m_2)

inv_logits <- as.data.frame(inv_logit(post$aS))

colnames(inv_logits) <- c("Москва", "СПб", "СтолицыРеспублик", "Регионы")

inv_logits[, "Москва-СПб"] <- inv_logits$Москва - inv_logits$СПб
inv_logits[, "Москва-СтолицыРеспублик"] <- inv_logits$Москва - inv_logits$СтолицыРеспублик
inv_logits[, "Москва-Регионы"] <- inv_logits$Москва - inv_logits$Регионы

inv_logits_wide <- inv_logits |>
  pivot_longer(values_to = "prob", cols=c("Москва-Регионы", "Москва-СПб", "Москва-СтолицыРеспублик"))

inv_logits_wide |>
  ggplot( aes(x=prob, fill=name)) +
  geom_histogram(color="#000000", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#F0E442", "#0072B2", "#009E73"),
                    labels=c("Москва-Регионы", "Москва-СПб", "Москва-СтолицыРеспублик")) +
  theme_minimal() +
  labs(x="Разность вероятностей употребления топонима родного\nрегиона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16),
        legend.text = element_text(size=12)) +
  theme(legend.position = c(1, 1.0625), legend.justification = c("right", "top")) +
  guides(fill=guide_legend(title = "")) +
  facet_wrap(~name)
ggsave("plot7.jpg", height=6, width=8)

inv_logits <- as.data.frame(inv_logit(post$aS))

colnames(inv_logits) <- c("Москва", "СПб", "СтолицыРеспублик", "Регионы")

inv_logits[, "СПб-Москва"] <- inv_logits$СПб - inv_logits$Москва
inv_logits[, "СПб-СтолицыРеспублик"] <- inv_logits$СПб - inv_logits$СтолицыРеспублик
inv_logits[, "СПб-Регионы"] <- inv_logits$СПб - inv_logits$Регионы

inv_logits_wide <- inv_logits |>
  pivot_longer(values_to = "prob", cols=c("СПб-Москва", "СПб-Регионы", "СПб-СтолицыРеспублик"))

inv_logits_wide |>
  ggplot( aes(x=prob, fill=name)) +
  geom_histogram(color="#000000", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#FC4E68", "#F0E442", "#009E73"),
                    labels=c("СПб-Москва", "СПб-Регионы", "СПб-СтолицыРеспублик")) +
  theme_minimal() +
  labs(x="Разность вероятностей употребления топонима родного\nрегиона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16),
        legend.text = element_text(size=12)) +
  theme(legend.position = c(1, 1.0625), legend.justification = c("right", "top")) +
  guides(fill=guide_legend(title = "")) +
  facet_wrap(~name)
ggsave("plot8.jpg", height=6, width=8)

inv_logits <- as.data.frame(inv_logit(post$aS))

colnames(inv_logits) <- c("Москва", "СПб", "СтолицыРеспублик", "Регионы")

inv_logits[, "Регионы-Москва"] <- inv_logits$Регионы - inv_logits$Москва
inv_logits[, "Регионы-СПб"] <- inv_logits$Регионы - inv_logits$СПб
inv_logits[, "Регионы-СтолицыРеспублик"] <- inv_logits$Регионы - inv_logits$СтолицыРеспублик

inv_logits_wide <- inv_logits |>
  pivot_longer(values_to = "prob", cols=c("Регионы-Москва", "Регионы-СПб", "Регионы-СтолицыРеспублик"))

inv_logits_wide |>
  ggplot( aes(x=prob, fill=name)) +
  geom_histogram(color="#000000", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#FC4E68", "#0072B2", "#009E73"),
                    labels=c("Регионы-Москва", "Регионы-СПб", "Регионы-СтолицыРеспублик")) +
  theme_minimal() +
  labs(x="Разность вероятностей употребления топонима родного\nрегиона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16),
        legend.text = element_text(size=12)) +
  theme(legend.position = c(1, 1.0625), legend.justification = c("right", "top")) +
  guides(fill=guide_legend(title = "")) +
  facet_wrap(~name)
ggsave("plot9.jpg", height=6, width=8)

inv_logits <- as.data.frame(inv_logit(post$aS))

colnames(inv_logits) <- c("Москва", "СПб", "СтолицыРеспублик", "Регионы")

inv_logits[, "СтолицыРеспублик-Москва"] <- inv_logits$СтолицыРеспублик - inv_logits$Москва
inv_logits[, "СтолицыРеспублик-СПб"] <- inv_logits$СтолицыРеспублик - inv_logits$СПб
inv_logits[, "СтолицыРеспублик-Регионы"] <- inv_logits$СтолицыРеспублик - inv_logits$Регионы

inv_logits_wide <- inv_logits |>
  pivot_longer(values_to = "prob", cols=c("СтолицыРеспублик-Москва", "СтолицыРеспублик-Регионы", "СтолицыРеспублик-СПб"))

inv_logits_wide |>
  ggplot( aes(x=prob, fill=name)) +
  geom_histogram(color="#000000", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#FC4E68", "#F0E442", "#0072B2"),
                    labels=c("СтолицыРеспублик-Москва", "СтолицыРеспублик-Регионы", "СтолицыРеспублик-СПб")) +
  theme_minimal() +
  labs(x="Разность вероятностей употребления топонима родного\nрегиона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16),
        legend.text = element_text(size=12)) +
  theme(legend.position = c(1, 1.0625), legend.justification = c("right", "top")) +
  guides(fill=guide_legend(title = "")) +
  facet_wrap(~name)
ggsave("plot10.jpg", height=6, width=8)

statuses <- data.frame("Москва"=inv_logit(post$aS[,1]), "СПб"=inv_logit(post$aS[,2]),
                       "СтолицыРеспублик"=inv_logit(post$aS[,3]),
                       "Регионы"=inv_logit(post$aS[,4]))

# Идея о языке литературы, восприятии литературной традиции региона, литература «деревенщиков»

statuses_wide <- statuses |>
  pivot_longer(values_to = "prob", cols=c("Москва", "СПб", "СтолицыРеспублик", "Регионы"))
statuses_wide |>
  ggplot( aes(x=prob, fill=name)) +
    geom_histogram(color="#000000", alpha=0.5, position = 'identity') +
    scale_fill_manual(values=c("#FC4E68", "#F0E442", "#0072B2", "#009E73"),
                      labels=c("Москва", "Регионы", "СПб", "СтолицыРеспублик")) +
  theme_minimal() +
  labs(x="Вероятность употребить топоним родного региона", y="Количество наблюдений") +
  theme(text=element_text(family="serif", size=16),
        legend.text = element_text(size=12)) +
  theme(legend.position = c(1, 1.0625), legend.justification = c("right", "top")) +
  guides(fill=guide_legend(title = "")) +
  facet_wrap(~name)
ggsave("plot5.jpg", height=6, width=8)

library(dagitty)

plant_dag <- dagitty( "dag {
                      M -> H
                      S -> H
                      S -> M
                      I -> H 
                      M -> I}")
coordinates( plant_dag ) <- list( x=c(H=1, M=1, S=1.5, I=0.5) ,
                                  y=c(H=0, M=0.5, S=0, I=0) )
adjustmentSets(x=plant_dag, exposure="M", outcome="H", effect="direct")

plot(plant_dag)

adjustmentSets(x=plant_dag, exposure="S", outcome="H", effect="direct")

plant_dag_2 <- dagitty( "dag {
                      M -> H
                      S -> H
                      S -> M}")
coordinates( plant_dag_2 ) <- list( x=c(H=1, M=1, S=1.5, I=0.5) ,
                                  y=c(H=0, M=0.5, S=0, I=0) )

plot(plant_dag_2)
