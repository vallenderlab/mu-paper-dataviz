# Creation of radar charts
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)
library(tibble)


rhc77 <- as.data.frame(read.csv(file="data/c77_triangle.csv", header=TRUE, sep=",", row.names = 1))



rhc77 %>%
  rownames_to_column( var = "ligand" ) %>%
  mutate_at(vars(-ligand),funs(rescale)) -> rhc77_radar

ggradar(rhc77_radar)