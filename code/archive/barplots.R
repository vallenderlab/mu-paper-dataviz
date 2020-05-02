# Barplots of reporter expression
library(ggpubr)
expression_data <- as.data.frame(read.csv(file="data/293_lentiviral.csv", header=TRUE, sep=","))