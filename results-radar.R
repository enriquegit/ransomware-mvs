library(dplyr)
library(ggpubr)
source("aux-functions.R")

# Path where the results were saved. Directory name with timestamp.
destpath <- "data/"

# Path to results_summary.csv
df <- read.csv("data/results_summary.csv")

df$method <- as.factor(df$method)

### Plot metrics.
nv <- 2 # Number of views to plot.

boxplot.pairwise(df, y="overall_f1", nv = nv)
boxplot.pairwise(df, y="overall_accuracy", nv = nv)
boxplot.pairwise(df, y="overall_precision", nv = nv)
boxplot.pairwise(df, y="overall_recall", nv = nv)


### Generate table.
t <- compute_overall_latex(df)

compute_summary_table_single(df, "f1")

