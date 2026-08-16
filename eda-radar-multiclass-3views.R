
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

# Add prefixes based on views. 2-48 v1, v2, 49-69 v3 (engineered)
features <- colnames(data)

# Create a copy
new_cols <- features

# 1. Add prefix "v1_" to elements starting with "winlog"
new_cols[grep("^winlog", new_cols)] <- paste0("v1_", new_cols[grep("^winlog", new_cols)])

# 2. Add prefix "v3_" to elements 49 to 69
new_cols[49:69] <- paste0("v3_", new_cols[49:69])

# 3. Add prefix "v2_" to all remaining elements except "class"
remaining <- setdiff(seq_along(new_cols), c(1, grep("^v1_", new_cols), 49:69))
new_cols[remaining] <- paste0("v2_", new_cols[remaining])

# Check the result
new_cols

colnames(data) <- new_cols

table(data$class)

write.csv(data, "data/radar-multiclass-3views/data.csv", row.names = F, quote = F)
