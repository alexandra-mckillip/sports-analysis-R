# ============================================================
# Performance Across Speedcubing Events Analysis
# Author: Alexandra McKillip
# Date: 2025-12-15
# Description:
#   Analysis of correlations across World Cube Association
#   events and predictions for competitor performances
# ============================================================

# ---- Libraries ----
library(tidyverse) # dplyr, tidyr, ggplot2, readr
library(softImpute)


# ---- Global Options ----

set.seed(12345)

# ---- Load Data ----

# load WCA competition results and competition data
results <- read_tsv("WCA_export_Results.tsv", show_col_types = FALSE)
competitions <- read_tsv("WCA_export_Competitions.tsv", show_col_types = FALSE)

# join result and competition data
results <- results |>
  left_join(competitions, by = c("competitionId" = "id"))

# ---- Data Cleaning ----

valid_events <- c("222", "333", "333oh", "444", "555", "666", "777",
                  "333bf", "333fm", "333mbf", "444bf", "555bf",
                  "clock", "minx", "pyram", "skewb", "sq1")

df <- results |>
  # remove retired events ("333ft", "333mbo", "magic", and "mmagic") and contaminated rows
  filter(eventId %in% valid_events) |>
  mutate(
    performance = case_when(
      # events where result is recorded as best attempt
      eventId %in% c("333bf", "333fm", "333mbf", "444bf", "555bf") ~ best,
      # events where result is recored as mean of 3 attempts
      eventId %in% c("222", "333", "333oh", "444", "555", "666", "777", "clock", "minx", "pyram", "skewb", "sq1") ~ average
    )
  ) |>
  # remove no result, DNF, and DNS rows
  filter(!performance %in% c(0, -1, -2)) |> # filter only keeps rows where condition is true
  # only cinlude data from 2020 to 2025
  filter(year >= 2020) |>
  select(personId, eventId, performance, year, month, day)

# only keep each competitors most recent year of performances
df <- df |>
  # add date key (YYYYMMDD format)
  mutate(date_key = year * 10000 + month * 100 + day) |>
  group_by(personId) |>
  # define last year of competition by subtracting one calendar year
  mutate(last_year = max(date_key) - 10000) |>
  filter(date_key >= last_year) |>
  # only keep competitors that compete in at least 4 events in their last year of competition
  filter(dplyr::n_distinct(eventId) >= 4) |>
  ungroup()

# calculate single average for each competitor in each event
df_avg <- df |>
  group_by(personId, eventId) |>
  summarise(
    avg_performance = mean(performance),
    .groups = "drop"
  )

# ---- Aggregation & Descriptives ----

# descriptive statistics (mean, variance, IQR for each event)
data_descriptive <- df_avg |>
  ungroup() |>
  mutate(
    perf = case_when(
      eventId %in% c("333mbf", "333fm") ~ avg_performance, # WCA points system or fewest moves
      TRUE ~ avg_performance / 100 # convert to seconds
    )
  ) |>
  group_by(eventId) |>
  # convert to seconds
  summarize(
    event_avg = round(mean(perf, na.rm = TRUE), 2),
    event_IQR = round(IQR(perf, na.rm = TRUE), 2),
    event_std = round(sd(perf, na.rm = TRUE), 2),
    num_competitors = dplyr::n(),
    .groups = "drop"
  )

# standardize data within each event
df_avg <- df_avg |>
  group_by(eventId) |>
  # timed and fewest moves events -> lower z score is better -> flip z score
  # multiblind event -> encoded score -> lower z score is better
  mutate(std_performance = -(avg_performance - mean(avg_performance)) / sd(avg_performance)) |>
  ungroup() |>
  select(personId, eventId, std_performance)

# pivot wider
df_avg_wide <- df_avg |>
  pivot_wider(
    names_from = eventId,
    values_from = std_performance
  )

# ---- Correlation Analysis ----

# pearson's coefficient matrix
cor_matrix <- cor(df_avg_wide[, -1], use = "pairwise.complete.obs", method = "pearson")

# mean of pearson matrix
mean(cor_matrix)

# event strengths
event_strength <- rowMeans(abs(cor_matrix))

event_strength_df <- data.frame(
  eventId = names(event_strength),
  strength = event_strength
) |>
  dplyr::arrange(desc(strength))

# save event strengths as csv
write.csv(
  event_strength_df,
  "event_strength.csv",
  row.names = FALSE
)

# ---- Visualization ----

# pearson correlation matrix heatmap
cor_df <- as.data.frame(as.table(cor_matrix))

cor_heatmap <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0
  ) +
  labs(title = "Pearson Correlation Heatmap of Speedcubing Events",
       x = "Event", y = "Event", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# save heatmap
ggsave("correlation_heatmap.png", plot = cor_heatmap, width = 8, height = 6)

# ---- Matrix Completion (SoftImpute) ----

# cross validation for to get optimal rank for matrix completion
set.seed(12345)

# convert to matrix (drop personId) and scale
X <- as.matrix(df_avg_wide[, -1])
X_scaled <- scale(X, center = TRUE, scale = TRUE)

# get positions of non-missing values
obs <- which(!is.na(X_scaled), arr.ind = TRUE)

# use 20% for testing data
val_idx <- obs[sample(nrow(obs), size = 0.2 * nrow(obs)), ]
X_train <- X_scaled
X_train[val_idx] <- NA

# lambda is the amount of shrinkage applied to singular values
lambda_max <- lambda0(X_train)
lambdas <- exp(seq(log(lambda_max), log(1e-2), length.out = 12))

rmse <- numeric(length(lambdas))
eff_rank <- numeric(length(lambdas))

# fit SoftImpute across the lambda grid and compute validation RMSE
for (i in seq_along(lambdas)) {
  fit <- softImpute(
    X_train,
    lambda = lambdas[i],
    type = "svd", #single value decomposition
    maxit = 1000
  )
  
  X_pred <- complete(X_train, fit)
  
  # get RMSE
  rmse[i] <- sqrt(mean((X_scaled[val_idx] - X_pred[val_idx])^2))
  
  # effective rank = number of non-zero singular values after shrinkage
  eff_rank[i] <- sum(fit$d > 1e-7)
}

# identify best rank and associated RMSE
best_idx <- which.min(rmse)
best_rank <- eff_rank[best_idx]

best_rank
rmse[best_idx]

# matrix completion on entire dataset
# convert to matrix and drop personId
X <- as.matrix(df_avg_wide[, -1])

# standardize each event column
X_scaled <- scale(X, center = TRUE, scale = TRUE)

fit <- softImpute(X_scaled, rank.max = best_rank, maxit = 500)

X_completed <- complete(X_scaled, fit)

# convert completed matrix to dataframe with competitor ids
X_final_df <- as.data.frame(X_completed)
colnames(X_final_df) <- colnames(df_avg_wide)[-1]
X_final_df <- cbind(personId = df_avg_wide$personId, X_final_df)

write_csv(X_final_df, "predicted_performances.csv")
