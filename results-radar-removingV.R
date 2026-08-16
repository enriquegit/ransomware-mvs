library(dplyr)
library(ggpubr)
source("aux-functions.R")

# Path to results_summary.csv without removing views.
df <- read.csv("data/d1/results_summary.csv")

# Path to results_summary.csv when removing a view.
df.removing <- read.csv("data/d2/results_summary.csv")

df$method <- as.factor(df$method)

df.removing$method <- as.factor(df.removing$method)


wilcox.test(df$overall_f1[which(df$method == "MVS")],
            df.removing$overall_f1[which(df.removing$method == "MVS")], paired = T)

wilcox.test(df$overall_accuracy[which(df$method == "MVS")],
            df.removing$overall_accuracy[which(df.removing$method == "MVS")], paired = T)

wilcox.test(df$overall_precision[which(df$method == "MVS")],
            df.removing$overall_precision[which(df.removing$method == "MVS")], paired = T)

wilcox.test(df$overall_recall[which(df$method == "MVS")],
            df.removing$overall_recall[which(df.removing$method == "MVS")], paired = T)


### v.s. AGG ####

wilcox.test(df$overall_f1[which(df$method == "AGG")],
            df.removing$overall_f1[which(df.removing$method == "MVS")], paired = T)

wilcox.test(df$overall_accuracy[which(df$method == "AGG")],
            df.removing$overall_accuracy[which(df.removing$method == "MVS")], paired = T)

wilcox.test(df$overall_precision[which(df$method == "AGG")],
            df.removing$overall_precision[which(df.removing$method == "MVS")], paired = T)

wilcox.test(df$overall_recall[which(df$method == "AGG")],
            df.removing$overall_recall[which(df.removing$method == "MVS")], paired = T)
