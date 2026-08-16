
# Path to the original file dataset-imbalanced-10-all.csv
df <- read.csv("C:/RADAR/dataset-imbalanced-10-all.csv")

# Clean class names.
df$target.class.name <- sub("-.*", "", df$target.class.name)

table(df$target.class.name) / nrow(df)

# 0:goodware 1:ransomware
table(df$target.class)

# Plot distribution.
t <- table(df$target.class.name)

barplot(t,
        main = "Class distribution",
        xlab = "", 
        ylab = "Counts", 
        col = "skyblue",
        border = "white",
        las = 2)

# Remove timestamp.
data <- df[,-1]

# Remove class.name
data <- data[,-70]

data <- cbind(class=data$target.class.name, data)

data <- data[,-70]

# Add prefixes. 2-48 v1, 49-69 v2 (engineered)
features <- colnames(data)

names.with.prefix <- c("class",paste0("v1_", features[2:48]),paste0("v2_", features[49:69]))

colnames(data) <- names.with.prefix

table(data$class)


write.csv(data, "data/radar-multiclass/data.csv", row.names = F, quote = F)
