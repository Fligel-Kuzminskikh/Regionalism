library(dplyr)
library(ggplot2)


d <- read.csv("C:\\Users\\User\\regionalism\\data\\n_toponyms.csv")
d_summarised <- d |>
  group_by(id) |>
  summarise(n_toponyms = sum(n))
d_summarised |>  
ggplot(aes(x=n_toponyms)) +
    geom_histogram(fill="#999999", color="#000000", alpha=0.5) +
    theme_minimal() +
  geom_vline(xintercept=mean(d_summarised$n_toponyms), linetype="dashed", linewidth=1,
             color="#000000") +
  geom_vline(xintercept=median(d_summarised$n_toponyms), linetype="dotted", linewidth=1,
             color="#000000") +
    labs(x="Количество топонимов", y="Количество книг") +
    theme(text=element_text(family="serif")) +
    theme(text=element_text(size=16))
ggsave("hist_toponyms.jpg", width=8, height=6)
