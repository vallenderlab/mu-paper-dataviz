# Barplots of reporter expression
library(tidyverse)
library(ggpubr)

# Import deltalog data
df <- as.data.frame(read.csv(file = "data/deltalogka.csv", header = TRUE, sep = ","))

# Filter data fram by only human data
humandf <- dplyr::filter(df, grepl("Human", X))

# Filter data fram by only monkey data
mmdf <- dplyr::filter(df, grepl("Monkey", X))



ligand_plot <- function(drug, species, color) {
  temp_C <- temp_K - 273.15
  return(temp_C)
}

ggplot(mmdf, aes(x = X, y = Morphine)) + geom_bar(position = position_dodge(.01), width = 0.2, stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = Morphine, ymax = Morphine+Morphine.SEM), width = .2, color = "black", size = 1, position = position_dodge(.4)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP", "Monkey JNK" = "JNK",
    "Monkey NFkB" = "NFkB"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )