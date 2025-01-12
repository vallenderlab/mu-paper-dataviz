# Barplots of delta logKa
# Copy and past in R console to run!!!!
library(tidyverse)
library(ggpubr)
library(forcats)
library(ggsignif)

# Import deltalog data
df <- as.data.frame(read.csv(file = "data/deltalogka_2019_01_25.csv", header = TRUE, sep = ","))

# columns
cols <- c("Morphine", "Met.enkephalin", "endomorphin.1", "b.endorphin", "TRV130")

# Filter data fram by only human data
humandf <- dplyr::filter(df, grepl("Human", X))
humandf$X <- factor(humandf$X, levels = humandf$X)

# Filter data fram by only monkey data
mmdf <- dplyr::filter(df, grepl("Monkey", X))
mmdf$X <- factor(mmdf$X, levels = mmdf$X)

# Monkey Morphine Plot
mmmor <- ggplot(mmdf, aes(x = mmdf$X, y = Morphine)) +
  geom_errorbar(aes(ymin = Morphine, ymax = Morphine-Morphine.SEM), width = .2, color = "#0000ff", position = position_dodge(.9)) +
  geom_bar(position = position_dodge(), stat = "identity", fill = "#0000ff", colour="#0000ff") +
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
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))

mmmor <- annotate_figure(mmmor, fig.lab = "F", fig.lab.face = "bold", fig.lab.size = 16)


ggsave("figures/2019_01_25/monkey_morphine.tiff", device = "tiff")

# Monkey Metenkephalin Plot
mmmet<- ggplot(mmdf, aes(x = fct_inorder(mmdf$X), y = mmdf$Met.enkephalin)) +
  geom_signif(annotations = c(formatC("*"),formatC("*")), y_position = c(2.5, 1.5), xmin=c(1, 1), xmax=c(3, 2)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b0f0", colour="#00b0f0") +
  geom_errorbar(data=mmdf[mmdf$Met.enkephalin > 0,], inherit.aes = F, aes(x= mmdf[mmdf$Met.enkephalin > 0,]$X, ymin = mmdf[mmdf$Met.enkephalin > 0,]$Met.enkephalin, ymax = mmdf[mmdf$Met.enkephalin > 0,]$Met.enkephalin+mmdf[mmdf$Met.enkephalin > 0,]$Met.enkephalin.SEM), width = .2, color = "#00b0f0", position = position_dodge(.9)) +
  geom_errorbar(data=mmdf[mmdf$Met.enkephalin < 0,], inherit.aes = F, aes(x= mmdf[mmdf$Met.enkephalin < 0,]$X, ymin = mmdf[mmdf$Met.enkephalin < 0,]$Met.enkephalin, ymax = mmdf[mmdf$Met.enkephalin < 0,]$Met.enkephalin-mmdf[mmdf$Met.enkephalin < 0,]$Met.enkephalin.SEM), width = .2, color = "#00b0f0", position = position_dodge(.9)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey NFkB" = "NF-κB",
    "Monkey JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Met-enkephalin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
mmmet<- annotate_figure(mmmet, fig.lab = "G", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/monkey_metenkephalin.tiff", device = "tiff")

# Monkey endomorphin.1 Plot
mmendo <- ggplot(mmdf, aes(x = fct_inorder(mmdf$X), y = mmdf$endomorphin.1)) +
  geom_signif(annotations = c(formatC("*"),formatC("*"), formatC("*")), y_position = c(2.75, .75, 1.75), xmin=c(1, 1, 2), xmax=c(3, 2, 3)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#ed7d27", colour="#ed7d27") +
  geom_errorbar(data=mmdf[mmdf$endomorphin.1 > 0,], inherit.aes = F, aes(x = mmdf[mmdf$endomorphin.1 > 0,]$X, ymin = mmdf[mmdf$endomorphin.1 > 0,]$endomorphin.1, ymax = mmdf[mmdf$endomorphin.1 > 0,]$endomorphin.1 + mmdf[mmdf$endomorphin.1 > 0,]$endomorphin.1.SEM), width = .2, color = "#ed7d27", position = position_dodge(.9)) +
  geom_errorbar(data=mmdf[mmdf$endomorphin.1 < 0,], inherit.aes = F, aes(x = mmdf[mmdf$endomorphin.1 < 0,]$X, ymin = mmdf[mmdf$endomorphin.1 < 0,]$endomorphin.1, ymax = mmdf[mmdf$endomorphin.1 < 0,]$endomorphin.1 - mmdf[mmdf$endomorphin.1 < 0,]$endomorphin.1.SEM), width = .2, color = "#ed7d27", position = position_dodge(.9)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey NFkB" = "NF-κB",
    "Monkey JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("Endomorphin-1") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
mmendo <- annotate_figure(mmendo, fig.lab = "H", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/monkey_endomorphin1.tiff", device = "tiff")

# Monkey beta-Endorphin Plot
mmbe <- ggplot(mmdf, aes(x = fct_inorder(mmdf$X), y = mmdf$b.endorphin)) +
  geom_signif(annotations = c(formatC("*"),formatC("*"), formatC("*")), y_position = c(2.5, .5, 1.5), xmin=c(1, 1, 2), xmax=c(3, 2, 3)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#9966ff", colour="#9966ff") +
  geom_errorbar(data=mmdf[mmdf$b.endorphin > 0,], inherit.aes = F, aes(x = mmdf[mmdf$b.endorphin > 0,]$X, ymin = mmdf[mmdf$b.endorphin > 0,]$b.endorphin, ymax = mmdf[mmdf$b.endorphin > 0,]$b.endorphin + mmdf[mmdf$b.endorphin > 0,]$b.endorphin.SEM), width = .2, color = "#9966ff", position = position_dodge(.9)) +
  geom_errorbar(data=mmdf[mmdf$b.endorphin < 0,], inherit.aes = F, aes(x = mmdf[mmdf$b.endorphin < 0,]$X, ymin = mmdf[mmdf$b.endorphin < 0,]$b.endorphin, ymax = mmdf[mmdf$b.endorphin < 0,]$b.endorphin - mmdf[mmdf$b.endorphin < 0,]$b.endorphin.SEM), width = .2, color = "#9966ff", position = position_dodge(.9)) +
  scale_x_discrete(labels = c(
    "Monkey cAMP" = "cAMP",
    "Monkey NFkB" = "NF-κB",
    "Monkey JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("β-Endorphin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
mmbe <- annotate_figure(mmbe, fig.lab = "I", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/monkey_betaendorphin.tiff", device = "tiff")

# Monkey TRV130 Plot
mmtrv <- ggplot(mmdf, aes(fct_inorder(mmdf$X), y = TRV130)) +
  geom_signif(annotations = c(formatC("*"),formatC("*"), formatC("*")), y_position = c(2.75, .75, 1.75), xmin=c(1, 1, 2), xmax=c(3, 2, 3)) +
  geom_errorbar(data=mmdf[mmdf$TRV130 > 0,], inherit.aes = F, aes(x= mmdf[mmdf$TRV130 > 0,]$X, ymin = mmdf[mmdf$TRV130 > 0,]$TRV130, ymax = mmdf[mmdf$TRV130 > 0,]$TRV130 + mmdf[mmdf$TRV130 > 0,]$TRV130.SEM), width = .2, color = "#00b050", position = position_dodge(.9)) +
  geom_errorbar(data=mmdf[mmdf$TRV130 < 0,], inherit.aes = F, aes(x= mmdf[mmdf$TRV130 < 0,]$X, ymin = mmdf[mmdf$TRV130 < 0,]$TRV130, ymax = mmdf[mmdf$TRV130 < 0,]$TRV130 - mmdf[mmdf$TRV130 < 0,]$TRV130.SEM), width = .2, color = "#00b050", position = position_dodge(.9)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b050", colour = "#00b050") +
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
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
mmtrv <- annotate_figure(mmtrv, fig.lab = "J", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/monkey_trv130.tiff", device = "tiff")

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

humanmor <- ggplot(humandf, aes(x = humandf$X, y = humandf$Morphine)) +
  geom_signif(annotations = c(formatC("*"),formatC("*")), y_position = c(1.5, 2.5), xmin=c(1, 2), xmax=c(3, 3)) +
  geom_bar(position = position_dodge(), stat = "identity", fill = "#0000ff",
           colour="#0000ff") +
  geom_errorbar(data=humandf[humandf$Morphine > 0,], inherit.aes = F, aes(x= humandf[humandf$Morphine > 0,]$X, ymin = humandf[humandf$Morphine > 0,]$Morphine, ymax = humandf[humandf$Morphine > 0,]$Morphine+humandf[humandf$Morphine > 0,]$Morphine.SEM), width = .2, color = "#0000ff", position = position_dodge(.9)) +
  geom_errorbar(data=humandf[humandf$Morphine < 0,], inherit.aes = F, aes(x= humandf[humandf$Morphine < 0,]$X, ymin = humandf[humandf$Morphine < 0,]$Morphine, ymax = humandf[humandf$Morphine < 0,]$Morphine-humandf[humandf$Morphine < 0,]$Morphine.SEM), width = .2, color = "#0000ff", position = position_dodge(.9)) +
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
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
humanmor <- annotate_figure(humanmor, fig.lab = "A", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/human_morphine.tiff", device = "tiff")

# Human Metenkephalin Plot
humanmet <- ggplot(humandf, aes(x = humandf$X, y = humandf$Met.enkephalin)) +
  geom_signif(annotations = c(formatC("*"),formatC("*")), y_position = c(1.5, 2.5), xmin=c(1, 2), xmax=c(2, 3)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b0f0", colour="#00b0f0") +
  geom_errorbar(data=humandf[humandf$Met.enkephalin > 0,], inherit.aes = F, aes(x= humandf[humandf$Met.enkephalin > 0,]$X, ymin = humandf[humandf$Met.enkephalin > 0,]$Met.enkephalin, ymax = humandf[humandf$Met.enkephalin > 0,]$Met.enkephalin+humandf[humandf$Met.enkephalin > 0,]$Met.enkephalin.SEM), width = .2, color = "#00b0f0", position = position_dodge(.9)) +
  geom_errorbar(data=humandf[humandf$Met.enkephalin < 0,], inherit.aes = F, aes(x= humandf[humandf$Met.enkephalin < 0,]$X, ymin = humandf[humandf$Met.enkephalin < 0,]$Met.enkephalin, ymax = humandf[humandf$Met.enkephalin < 0,]$Met.enkephalin-humandf[humandf$Met.enkephalin < 0,]$Met.enkephalin.SEM), width = .2, color = "#00b0f0", position = position_dodge(.9)) +
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
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
humanmet <- annotate_figure(humanmet, fig.lab = "B", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/human_metenkephalin.tiff", device = "tiff")

# Human endomorphin.1 Plot
humanendo <- ggplot(humandf, aes(x = humandf$X, y = humandf$endomorphin.1)) +
  geom_signif(annotations = c(formatC("*"),formatC("*")), y_position = c(1.5, 2.5), xmin=c(1, 2), xmax=c(3, 3)) +
  geom_bar(width = 0.6, stat = "identity", fill = "#ed7d27", colour="#ed7d27") +
  geom_errorbar(data=humandf[humandf$endomorphin.1 > 0,], inherit.aes = F, aes(x= humandf[humandf$endomorphin.1 > 0,]$X, ymin = humandf[humandf$endomorphin.1 > 0,]$endomorphin.1, ymax = humandf[humandf$endomorphin.1 > 0,]$endomorphin.1 + humandf[humandf$endomorphin.1 > 0,]$endomorphin.1.SEM), width = .2, color = "#ed7d27") +
  geom_errorbar(data=humandf[humandf$endomorphin.1 < 0,], inherit.aes = F, aes(x= humandf[humandf$endomorphin.1 < 0,]$X, ymin = humandf[humandf$endomorphin.1 < 0,]$endomorphin.1, ymax = humandf[humandf$endomorphin.1 < 0,]$endomorphin.1 - humandf[humandf$endomorphin.1 < 0,]$endomorphin.1.SEM), width = .2, color = "#ed7d27") +
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
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
humanendo <- annotate_figure(humanendo, fig.lab = "C", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/human_endomorphin1.tiff", device = "tiff")

# Human beta-Endorphin Plot
humanbe <- ggplot(humandf, aes(x = humandf$X, y = humandf$b.endorphin)) +
  geom_signif(annotations = c(formatC("*"),formatC("*"), formatC("*")), y_position = c(2.5, .5, 1.5), xmin=c(1, 1, 2), xmax=c(3, 2, 3)) +
  geom_errorbar(data=humandf[humandf$b.endorphin < 0,], inherit.aes = F, aes(x= humandf[humandf$b.endorphin < 0,]$X, ymin = humandf[humandf$b.endorphin < 0,]$b.endorphin, ymax = humandf[humandf$b.endorphin < 0,]$b.endorphin - humandf[humandf$b.endorphin < 0,]$b.endorphin.SEM), width = .2, color = "#9966ff", position = position_dodge(.9)) +
  geom_errorbar(data=humandf[humandf$b.endorphin > 0,], inherit.aes = F, aes(x= humandf[humandf$b.endorphin > 0,]$X, ymin = humandf[humandf$b.endorphin > 0,]$b.endorphin, ymax = humandf[humandf$b.endorphin > 0,]$b.endorphin + humandf[humandf$b.endorphin > 0,]$b.endorphin.SEM), width = .2, color = "#9966ff", position = position_dodge(.9)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#9966ff", colour="#9966ff") +
  scale_x_discrete(labels = c(
    "Human cAMP" = "cAMP",
    "Human NFkB" = "NF-κB",
    "Human JNK" = "JNK"
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + ggtitle("β-Endorphin") +
  xlab(NULL) + ylab("ΔLog(τ/KA)") + theme_pubclean() +
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
humanbe <- annotate_figure(humanbe, fig.lab = "D", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/human_betaendorphin.tiff", device = "tiff")

# Human TRV130 Plot
humantrv <- ggplot(humandf, aes(x = X, y = TRV130)) +
  geom_signif(annotations = c(formatC("*"),formatC("*")), y_position = c(1.5, 2.5), xmin=c(1, 2), xmax=c(2, 3)) +
  geom_errorbar(data=humandf[humandf$TRV130 > 0,], inherit.aes = F, aes(x= humandf[humandf$TRV130 > 0,]$X, ymin = humandf[humandf$TRV130 > 0,]$TRV130, ymax = humandf[humandf$TRV130 > 0,]$TRV130 + humandf[humandf$TRV130 > 0,]$TRV130.SEM), width = .2, color = "#00b050", position = position_dodge(.9)) +
  geom_errorbar(data=humandf[humandf$TRV130 < 0,], inherit.aes = F, aes(x= humandf[humandf$TRV130 < 0,]$X, ymin = humandf[humandf$TRV130 < 0,]$TRV130, ymax = humandf[humandf$TRV130 < 0,]$TRV130 - humandf[humandf$TRV130 < 0,]$TRV130.SEM), width = .2, color = "#00b050", position = position_dodge(.9)) +
  geom_bar(position = position_dodge(.01), width = 0.6, stat = "identity", fill = "#00b050", colour = "#00b050") +
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
  scale_y_continuous(breaks=c(-3,-2, -1, 0, 1, 2, 3), limits=c(-3, 3)) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))

humantrv <- annotate_figure(humantrv, fig.lab = "E", fig.lab.face = "bold", fig.lab.size = 16)

ggsave("figures/2019_01_25/human_trv130.tiff", device = "tiff")

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

### Arrange plots

arranged <- ggarrange(humanmor, humanmet, humanendo, humanbe, humantrv,
                      mmmor, mmmet, mmendo, mmbe, mmtrv,
                      common.legend = TRUE, ncol = 5, nrow = 2)

ggsave("figures/2019_01_25/arranged_plots_deltalogka.tiff", width = 15, height = 7, dpi = 300, device = "tiff")