# Barplots of ΔlogKa
library(tidyverse)
library(ggpubr)

# Import deltalog data
df <- as.data.frame(read.csv(file = "data/deltalogka.csv", header = TRUE, sep = ","))

# columns
cols <- c("Morphine", "Met.enkephalin", "endomorphin.1", "b.endorphin", "TRV130")

# Filter data fram by only human data
humandf <- dplyr::filter(df, grepl("Human", X))

# Filter data fram by only monkey data
mmdf <- dplyr::filter(df, grepl("Monkey", X))

# Monkey Morphine Plot
ggplot(mmdf, aes(x = X, y = Morphine)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = Morphine - Morphine.SEM, ymax = Morphine + Morphine.SEM), width = .2, color = "black", size = 1, position = position_dodge(.4)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP", "Monkey JNK" = "JNK",
    "Monkey NFkB" = "NFkB"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Morphine") +
  xlab(NULL) + ylab("Δ logKa") + theme_pubclean()

ggsave("output/monkey_morphine.tiff", device = "tiff")

# Monkey Metenkephalin Plot
ggplot(mmdf, aes(x = mmdf$X, y = mmdf$Met.enkephalin)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "green") +
  geom_errorbar(aes(ymin = Met.enkephalin - Met.enkephalin.SEM, ymax = Met.enkephalin + Met.enkephalin.SEM), width = .2, color = "black", size = 1, position = position_dodge(.4)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP", "Monkey JNK" = "JNK",
    "Monkey NFkB" = "NFkB"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Met-enkephalin") +
  xlab(NULL) + ylab("Δ logKa") + theme_pubclean()

ggsave("output/monkey_metenkephalin.tiff", device = "tiff")
