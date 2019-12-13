#
#
#

# ---- Required Packages ----

require(tidyverse)


# ---- Data Import ----

#' Import data from file.
#' Create sex binary column for later use.
#'
#' @param filename a csv file
#'
#' @return a tibble of read data
#'
#' @examples
#' read_data("titanic_group_project.csv")
read_data <- function(filename) {
  temp_data <- read_csv(filename)
  temp_data$sex <- as.character(temp_data$sex)
  temp_data <- temp_data %>% mutate(sex_binary = ifelse(sex == "female", 1, 0))

  return(temp_data)
}

# ---- Task 1 ----

#
#
#

# make a default logistic model from the dataset
default_logistic_model <- function(data_set) {

  model <- glm(
    formula = survived ~ pclass + sex_binary + age,
    data = data_set,
    family = binomial
  )
  
  return(model)

}

# gets the probability of survival for specified values
# takes an already-prepared model, or makes a default one
prob_survival <- function(model = NULL, data_set = NULL, pclass = 1, sex_binary = 1, age = 1) {

  # make a default model if none is provided
  if (is.null(model)) {
    model <- default_logistic_model(data_set)
  }
  
  log_odds_survival <- (summary(model)$coefficients[1,1]) +
    (summary(model)$coefficients[2,1] * pclass) +
    (summary(model)$coefficients[3,1] * sex_binary)+
    (summary(model)$coefficients[4,1] * age)

    probs_survival <- exp(log_odds_survival) / (1 + (exp(log_odds_survival)))

    return(probs_survival)

    }

# ---- Task 2 ----

survival_stats <- function(data_set, model = NULL) {
  
  # make a default model if none is provided
  if (is.null(model)) {
    log_model <- default_logistic_model(data_set)
  }

  # calculate the odds of survival for every case in a dataset
  new_data <- data_set %>%
    mutate(survival.prob = prob_survival(
      model = model,
      data_set = data_set,
      data_set$pclass,
      data_set$sex_binary,
      data_set$age
    ))

  return(new_data)
}

# takes set with already made survival probabilities
survival_plots <- function(survival_data, nbin = 30) {
  
  survival_data <- survival_data %>% 
    drop_na("survival.prob")

  plt <- ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$survival.prob),
      bins = nbin, color = "white"
    ) +
    labs(x = "probability of survival")
  
  return(plt)

}

# takes set with already made survival probabilities
# expects a string form of column name
plots_by <- function (survival_data, colname, nbin = 30) {
  
  survival_data <- survival_data %>% 
    drop_na("survival.prob")
  
  ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$survival.prob),
      bins = nbin, color = "white"
    ) +
    labs(x = "probability of survival") +
    facet_wrap(colname)
}

# ---- Task 3 ----

# predict survival using cutoff value
survival_predict <- function(survival_data, cutoff_val = 0.51) {

  w_cutoff <- survival_data %>% 
    mutate(survival.pred = ifelse(survival.prob > cutoff_val, 1, 0))

  return(w_cutoff)
  
}

# compare model to reality, expects set with predictions
compare_probs_survival <- function(survival_data, cutoff_val = 0.51) {
  
  survival_pred <- survival_predict(survival_data, cutoff_val)

  # new value , 1 if survival prediction matches reality else 0
  survival_pred <- survival_pred %>% 
    mutate(pred.correct = ifelse(survival.pred == survived, 1, 0))

  return(survival_pred)
  
}

# get percent of correct predictions
# expects set with comparisons made
percent_survival_correct <- function(data_set) {
  
  data_set <- data_set %>% 
    drop_na("pred.correct")

  pcorrect <- 100 * sum(data_set$pred.correct) / nrow(data_set)

  return(pcorrect)
  
}

# compare the accuracy of a range of cutoff values
cutoff_compare <- function(survival_data, c.range = c(0, 1), n.by = 0.01) {
  
  temp_range = seq(from = c.range[1], to = c.range[2], by = n.by)
  
  test_vals <- tibble(
    c.val = temp_range,
    p.correct = rep(NA, length(temp_range))
  )
  
  percentages <- rep(NA, length(temp_range))
  
  for (i in 1:length(temp_range)) {
    predictions <- compare_probs_survival(survival_data, temp_range[i])
    percentages[i] <- percent_survival_correct(predictions)
  }
  
  test_vals$p.correct <- percentages
  
  cutoff_plot <- ggplot(test_vals) +
    aes(x = c.val, y = p.correct, color = p.correct) +
    geom_point() +
    geom_smooth() +
    labs(x = "cutoff value", y = "percent correct") +
    scale_x_continuous(breaks = seq(c.range[1], c.range[2], len = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(
      yintercept = max(test_vals$p.correct),
      color = "green"
    ) +
    geom_vline(
      xintercept = test_vals$c.val[test_vals$p.correct == max(test_vals$p.correct)],
      color = "green"
    ) +
    geom_text(
      aes(
        x = test_vals$c.val[test_vals$p.correct == max(test_vals$p.correct)],
        y = 50,
        label = test_vals$c.val[test_vals$p.correct == max(test_vals$p.correct)]
      ),
      color = "black"
    )
  
  return(cutoff_plot)
  
} 

