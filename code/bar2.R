# Barplots of reporter expression
library(tidyverse)
library(ggpubr)

# Import Data
jnk_nfkb <- as.data.frame(read.csv(file="data/xiao_mu_jnkvsnfkb.csv", header=TRUE, sep=","))

# Tidy data for bar plot
jnk_nfkb %>% tidyr::gather("id", "value", 2:5) %>%
  ggplot(., aes(id, value)) + geom_bar(stat = "identity", aes(fill = Ligand), position = "dodge") +
  xlab("Pathway") + ylab(NULL) +
  ggtitle("Ligand Bias Factor") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank()) +
  scale_x_discrete(labels=c("Monkey.JNK" = "Monkey JNK", "Monkey.NFKB" = "Monkey NFkB",
                                            "Human.JNK" = "Human JNK", "Human.NFKB" = "Human NFkB"))