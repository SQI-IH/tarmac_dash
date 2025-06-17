# ---- train_model.R -----------------------------------------------------
# Trains a logistic regression model to predict whether a physician
# will receive a patient during a specific time block
parse_ed_time <- function(date_vec) {
  # Try known formats in order of likelihood
  parsed <- lubridate::parse_date_time(
    date_vec,
    orders = c("ymd HMS", "ymd HM", "ymd", "mdy HMS", "mdy HM", "mdy",
               "dmy HMS", "dmy HM", "dmy", "Ymd HMS", "Ymd"),
    exact = FALSE,
    tz = "UTC"
  )
  return(parsed)
}
library(caret)

pkgs <- c("ggplot2", "plyr", "dplyr","plotly",
"tidyverse", "lubridate", "forcats", "RColorBrewer")
for(i in 1:length(pkgs)){
is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(pkgs[i])){install.packages(pkgs[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
  library(pkgs[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}


# -------------------------------------------------------------------------
# ---- Load Data ----
# Replace this with your actual data loading code
fls <- list.files('./app_data/ed/', pattern = "*.csv", full.names = TRUE)
df <- fls[!grepl('Royal Inland', fls)] |>
  lapply(data.table::fread) |>
  lapply(function(x) {
    # Standardize the 'lha' column to character
    x[] <- lapply(x, as.character)
    return(x)
  }) |>
  dplyr::bind_rows()
# dfBk <- df

# ---- Load Required Libraries ----
library(dplyr)
library(tidyr)
library(lubridate)
library(caret)
library(xgboost)
library(purrr)
library(tibble)
library(readr)

# ---- Step 1: Enrich Raw Data into Time Blocks ----
df_blocked <- df %>%
  mutate(
    ed_arrival_date_time = parse_ed_time(ed_arrival_date_time),
    hour_block = hour(ed_arrival_date_time),
    wday = wday(ed_arrival_date_time, label = TRUE),
    is_weekend = wday %in% c("Sat", "Sun"),
    arrived_by_ground_ambulance = str_to_lower(arrival_mode) == "ground ambulance"
  ) %>%
  group_by(facility_name, hour_block, wday, is_weekend) %>%
  summarise(
    arrivals_last_3h = n(),
    ctas2_amb_last_3h = sum(ctas_level == 2 & arrived_by_ground_ambulance, na.rm = TRUE),
    ctas3_amb_last_3h = sum(ctas_level == 3 & arrived_by_ground_ambulance, na.rm = TRUE),
    ctas4_amb_last_3h = sum(ctas_level == 4 & arrived_by_ground_ambulance, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Step 2: Train Models for CTAS 2, 3, 4 ----
dir.create("models", showWarnings = FALSE)
dir.create("thresholds", showWarnings = FALSE)

ctas_levels <- c(2, 3, 4)

for (ctas in ctas_levels) {
  message("Training model for CTAS ", ctas, "...")
  
  df_model <- df_blocked %>%
    mutate(
      outcome = if_else(get(paste0("ctas", ctas, "_amb_last_3h")) > 0, "yes", "no"),
      outcome = factor(outcome, levels = c("no", "yes"))
    ) %>%
    select(outcome, facility_name, hour = hour_block, wday, is_weekend, arrivals_last_3h) %>%
    na.omit()
  
  set.seed(42)
  index <- createDataPartition(df_model$outcome, p = 0.8, list = FALSE)
  train_df <- df_model[index, ]
  test_df <- df_model[-index, ]
  
  ctrl <- trainControl(
    method = "cv", number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "up"
  )
  
  xgb_model <- train(
    outcome ~ ., data = train_df,
    method = "xgbTree",
    metric = "ROC",
    trControl = ctrl
  )
  
  test_df$pred_prob <- predict(xgb_model, newdata = test_df, type = "prob")[, "yes"]
  
  thresholds <- seq(0.05, 0.95, by = 0.01)
  threshold_results <- map_dfr(thresholds, function(t) {
    preds <- ifelse(test_df$pred_prob > t, "yes", "no")
    cm <- confusionMatrix(
      factor(preds, levels = c("no", "yes")),
      test_df$outcome,
      positive = "yes"
    )
    tibble(threshold = t, balanced_accuracy = cm$byClass["Balanced Accuracy"])
  })
  
  best_thresh <- threshold_results %>%
    filter(balanced_accuracy == max(balanced_accuracy, na.rm = TRUE)) %>%
    pull(threshold) %>%
    first()
  
  saveRDS(xgb_model, file = paste0("models/ctas", ctas, "_model.rds"))
  write_rds(best_thresh, file = paste0("thresholds/ctas", ctas, "_threshold.rds"))
  
  message("✅ CTAS ", ctas, " model and threshold saved.")
}
