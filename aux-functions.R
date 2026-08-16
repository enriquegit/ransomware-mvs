library(dplyr)
library(tidyr)
library(stringr)
library(xtable)

boxplot.pairwise <- function(df, y="overall_f1", nv = 2,
                             width=7, height=4.4){
  
  my_comparisons <- list( c("MVS", "AGG"), c("MVS", "V1"), c("MVS", "V2"))
  
  if(nv==3){
    my_comparisons <- list( c("MVS", "AGG"), c("MVS", "V1"), c("MVS", "V2"), c("MVS", "V3"))
  }
  else if(nv==4){
    my_comparisons <- list( c("MVS", "AGG"), c("MVS", "V1"), c("MVS", "V2"), c("MVS", "V3"), c("MVS", "V4"))
  }
  
  p <- ggboxplot(df, x = "method", y = y,
                 color = "method", palette = "jco") +
    ggtitle("") + xlab("model type") + ylab(y) +
    stat_compare_means(comparisons = my_comparisons,
                       method = "wilcox.test",
                       paired = T,
                       method.args = list(alternative = "greater", paired=T),
                       label = "p.format")
  
  
  fname <- paste0("boxplots_",y,".pdf")
  
  pdf(paste0(destpath,fname),width=width, height=height)
  
  print(p)
  
  dev.off()
  
}

compute_summary_table <- function(df) {

  # Identify metric columns (everything except 'it' and 'method')
  metric_cols <- setdiff(names(df), c("it", "method"))
  
  # Reshape into long format
  df_long <- df %>%
    pivot_longer(cols = all_of(metric_cols),
                 names_to = "class_metric",
                 values_to = "value") %>%
    separate(class_metric, into = c("class", "metric"), sep = "_")
  
  # Compute mean and sd per method, class, metric
  summary_stats <- df_long %>%
    group_by(method, class, metric) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Compute overall mean and sd per method
  overall_stats <- df_long %>%
    group_by(method, metric) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      class = "Overall",
      .groups = "drop"
    )
  
  # Combine class-level and overall
  combined <- bind_rows(summary_stats, overall_stats)
  
  # Create formatted text for LaTeX (mean ± sd)
  combined <- combined %>%
    mutate(Result = sprintf("%.3f $\\pm$ %.3f", mean, sd)) %>%
    select(method, class, metric, Result) %>%
    pivot_wider(names_from = metric, values_from = Result)
  
  # Convert to LaTeX table
  latex_table <- xtable(combined, caption = "Mean and standard deviation per class and metric")
  
  print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)
  
  return(combined)
}

compute_summary_table_single <- function(df, selected_metric) {
  
  # Identify class-metric columns
  metric_cols <- setdiff(names(df), c("it", "method"))
  
  # Reshape into long format
  df_long <- df %>%
    pivot_longer(cols = all_of(metric_cols),
                 names_to = "class_metric",
                 values_to = "value") %>%
    separate(class_metric, into = c("class", "metric"), sep = "_")
  
  # Filter by selected metric and exclude 'overall'
  df_filtered <- df_long %>%
    filter(metric == selected_metric, class != "overall")
  
  if (nrow(df_filtered) == 0) {
    stop(paste0("No valid class entries found for metric: ", selected_metric))
  }
  
  # Compute mean and sd per method and class
  summary_stats <- df_filtered %>%
    group_by(method, class) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Format as mean ± sd (3 decimals)
  summary_stats <- summary_stats %>%
    mutate(Result = sprintf("%.3f $\\pm$ %.3f", mean, sd)) %>%
    select(class, method, Result) %>%
    pivot_wider(names_from = method, values_from = Result)
  
  # Generate LaTeX table (classes as rows)
  latex_table <- xtable(summary_stats,
                        caption = paste("Mean ± SD for metric:", selected_metric, "(excluding overall)"))
  
  print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)
  
  return(summary_stats)
}


compute_overall_latex <- function(df) {
  
  # Select only the overall metric columns
  metric_cols <- setdiff(names(df), c("it", "method"))
  overall_cols <- metric_cols[str_detect(metric_cols, "^overall_")]
  
  if (length(overall_cols) == 0) {
    stop("No 'overall_' columns found in the dataset.")
  }
  
  # Reshape into long format
  df_long <- df %>%
    select(method, all_of(overall_cols)) %>%
    pivot_longer(cols = all_of(overall_cols),
                 names_to = "metric",
                 values_to = "value") %>%
    mutate(metric = str_remove(metric, "^overall_"))
  
  # Compute mean and standard deviation by method and metric
  summary_stats <- df_long %>%
    group_by(method, metric) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Format the result as mean ± sd
  summary_stats <- summary_stats %>%
    mutate(Result = sprintf("%.3f $\\pm$ %.3f", mean, sd)) %>%
    select(method, metric, Result) %>%
    pivot_wider(names_from = metric, values_from = Result)
  
  # Generate LaTeX table
  latex_table <- xtable(summary_stats, caption = "Overall performance (mean ± sd) across metrics")
  print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)
  
  return(summary_stats)
}

