# Barplots of ΔlogKa
library(tidyverse)
library(ggpubr)

# Import deltalog data
df <- as.data.frame(read.csv(file = "data/deltalogka.csv", header = TRUE, sep = ","))

# columns
cols <- c("Morphine", "Met.enkephalin", "endomorphin.1", "b.endorphin", "TRV130")

# Filter data fram by only human data
humandf <- dplyr::filter(df, grepl("Human", X))
humandf$X <- factor(humandf$X, levels = c("Human cAMP", "Human NFkB", "Human JNK"))

# Filter data fram by only monkey data
mmdf <- dplyr::filter(df, grepl("Monkey", X))
mmdf$X <- factor(mmdf$X, levels = c("Monkey cAMP", "Monkey NFkB", "Monkey JNK"))

# Monkey Morphine Plot
# Correctly formatted!!!
ggplot(mmdf, aes(x = X, y = Morphine)) +
  geom_errorbar(aes(ymin = Morphine, ymax = Morphine-Morphine.SEM), width = .2,
                color = "#0000ff", position = position_dodge(.9)) +
  geom_bar(position = position_dodge(), stat = "identity", fill = "#0000ff",
           colour="#0000ff") +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey NFkB" = "NF-κB",
    "Monkey JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Morphine") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/monkey_morphine.tiff", device = "tiff")

# Monkey Metenkephalin Plot
ggplot(mmdf, aes(x = mmdf$X, y = mmdf$Met.enkephalin)) +
  geom_errorbar(aes(ymin = Met.enkephalin, ymax = Met.enkephalin-Met.enkephalin.SEM), width = .2, color = "#00b0f0", position = position_dodge(.9)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b0f0", colour="#00b0f0") +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey JNK" = "JNK",
    "Monkey NFkB" = "NF-κB"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Met-enkephalin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/monkey_metenkephalin.tiff", device = "tiff")

# Monkey endomorphin.1 Plot
ggplot(mmdf, aes(x = X, y = endomorphin.1)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#ed7d27") +
  geom_errorbar(aes(ymin = endomorphin.1, ymax = endomorphin.1-endomorphin.1.SEM), width = .2, color = "black", size = 1, position = position_dodge(.05)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey NFkB" = "NFkB",
    "Monkey JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Endomorphin-1") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/monkey_endomorphin1.tiff", device = "tiff")

# Monkey beta-Endorphin Plot
ggplot(mmdf, aes(x = mmdf$X, y = mmdf$b.endorphin)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#9966ff") +
  geom_errorbar(aes(ymin = b.endorphin, ymax = b.endorphin-b.endorphin.SEM), width = .2, color = "black", size = 1, position = position_dodge(.4)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP", "Monkey JNK" = "JNK",
    "Monkey NFkB" = "NF-κB"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("beta-Endorphin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/monkey_betaendorphin.tiff", device = "tiff")

# Monkey TRV130 Plot
ggplot(mmdf, aes(x = X, y = TRV130)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b050") +
  geom_errorbar(aes(ymin = TRV130, ymax = TRV130-TRV130.SEM), width = .2, color = "black", size = 1, position = position_dodge(.05)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey NFkB" = "NF-κB",
    "Monkey JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("TRV130") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/monkey_trv130.tiff", device = "tiff")

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Human Morphine Plot
ggplot(humandf, aes(x = X, y = Morphine)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#0000ff") +
  geom_errorbar(aes(ymin = Morphine, ymax = Morphine-Morphine.SEM), width = .2, color = "black", size = 1, position = position_dodge(.05)) +
  scale_x_discrete(labels = c(
    "Human cAMP" = "cAMP",
    "Human NFkB" = "NF-κB",
    "Human JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Morphine") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/human_morphine.tiff", device = "tiff")

# Human Metenkephalin Plot
ggplot(humandf, aes(x = mmdf$X, y = humandf$Met.enkephalin)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b0f0") +
  geom_errorbar(aes(ymin = Met.enkephalin, ymax = Met.enkephalin-Met.enkephalin.SEM), width = .2, color = "black", size = 1, position = position_dodge(.4)) +
  scale_x_discrete(labels = c(
    "Human cAMP" = "cAMP",
    "Human NFkB" = "NF-κB",
    "Human JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Met-enkephalin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/human_metenkephalin.tiff", device = "tiff")

# Human Endomorphin-1 Plot
ggplot(humandf, aes(x = X, y = endomorphin.1)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#ed7d27") +
  geom_errorbar(aes(ymin = endomorphin.1, ymax = endomorphin.1-endomorphin.1.SEM), width = .2, color = "black", size = 1, position = position_dodge(.05)) +
  scale_x_discrete(labels = c(
    "Human cAMP" = "cAMP",
    "Human NFkB" = "NF-κB",
    "Human JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Endomorphin-1") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/human_endomorphin1.tiff", device = "tiff")

# Human beta-Endorphin Plot
ggplot(humandf, aes(x = mmdf$X, y = humandf$b.endorphin)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#9966ff") +
  geom_errorbar(aes(ymin = b.endorphin, ymax = b.endorphin-b.endorphin.SEM), width = .2, color = "black", size = 1, position = position_dodge(.4)) +
  scale_x_discrete(labels = c(
    "Human cAMP" = "cAMP",
    "Human NFkB" = "NF-κB",
    "Human JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("beta-Endorphin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/human_betaendorphin.tiff", device = "tiff")

# Human TRV130 Plot
ggplot(humandf, aes(x = X, y = TRV130)) + geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b050") +
  geom_errorbar(aes(ymin = TRV130, ymax = TRV130-TRV130.SEM), width = .2, color = "black", size = 1, position = position_dodge(.05)) +
  scale_x_discrete(labels = c(
    "Human cAMP" = "cAMP",
    "Human NFkB" = "NF-κB",
    "Human JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("TRV130") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3))

ggsave("figures/human_trv130.tiff", device = "tiff")
