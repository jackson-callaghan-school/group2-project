# Jackson Callaghan, Derrick Liu, Charlotte Chase
# titanic_survival.R
# Tools for modeling survival probabilities in titanic or (very) similar data
# Work:
#   Idea and data gathering by Charlotte. 
#   Writing work split up. 
#   Documentation split up.
#   Bugfixing by Derrick.
#   Final cleaning & formatting by Jackson.

# ---- Required Packages ----

require(tidyverse) # used for operating on, transforming, and visualizing data

# ---- Data Import ----

#' Import data from file.
#' Create sex binary column for later use.
#'
#' @param filename a csv file of similar format to example titanic data.
#'
#' @return a tibble of read data.
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

# Create a model of survival rates
# Use a given model to calculate survival probability
# Calculate all survival probabilities in a dataframe given a model


#' Create a default logistic model from a dataset.
#'
#' @param data_set A tibble read in using read_data.
#'
#' @return a logistic regression model.
#'
#' @examples
#' default_logistic_model(my_data)
default_logistic_model <- function(data_set) {

  model <- glm(
    formula = survived ~ pclass + sex_binary + age,
    data = data_set,
    family = binomial
  )
  
  return(model)

}

#' Calculate probability of survival for given values using given model.
#'
#' @param model A logistic regression model of pclass, sex_binary, and age.
#' @param data_set A tibble to generate a model from if no model is given.
#' @param pclass Passenger class, numeric from 1 to 3. Defaults to 1.
#' @param sex_binary Sex, 1 if female, 0 if male. Defaults to 1.
#' @param age Age in years. Defaults to 1.
#'
#' @return A numeric probability of survival
#'
#' @examples
#' prob_survival(model = my_model, pclass = 1, sex_binary = 0, age = 45)
#' prob_survival(data_set = my_data, pclass = 3, sex_binary = 1, age = 26)
prob_survival <- function(model = NULL, data_set = NULL, pclass = 1, sex_binary = 1, age = 1) {

  # make a default model if none is provided
  if (is.null(model)) {
    model <- default_logistic_model(data_set)
  }
  
  # get coefficients from model
  log_odds_survival <- (summary(model)$coefficients[1,1]) +
    (summary(model)$coefficients[2,1] * pclass) +
    (summary(model)$coefficients[3,1] * sex_binary)+
    (summary(model)$coefficients[4,1] * age)

  # compute probability
  probs_survival <- exp(log_odds_survival) / (1 + (exp(log_odds_survival)))

  return(probs_survival)

}

# ---- Task 2 ----

# Calculate survival probabilites for all cases in a dataset
# Plot survival probabilites
# Plot surival probabilites, faceted by a given variable

#' Calculate survival probabilities for all cases in a given dataset using a given model.
#'
#' @param data_set A tibble read in using read_data.
#' @param model A logistic regression model. A default will be generated if none is given.
#'
#' @return The original tibble with a variable for probability of survival survival.prob.
#'
#' @examples
#' survival_stats(my_data)
#' survival_stats(my_data, model = my_model)
survival_stats <- function(data_set, model = NULL) {
  
  # make a default model if none is provided
  if (is.null(model)) {
    log_model <- default_logistic_model(data_set)
  }

  # calculate the probability of survival for every case in a dataset
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

#' Plot histogram of surivival probabilities.
#'
#' @param survival_data A tibble with survival probabilities in variable named survival.prob.
#' @param nbin Number of bins to use in histogram. Defaults to 30.
#'
#' @return A ggplot2 histogram of probabilites.
#'
#' @examples
#' survival_plots(my_data, 20)
#' survival_plots(my_data)
survival_plots <- function(survival_data, nbin = 30) {
  
  # drop NA values
  survival_data <- survival_data %>% 
    drop_na("survival.prob")

  # create plot
  plt <- ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$survival.prob),
      bins = nbin, color = "white"
    ) +
    labs(x = "probability of survival")
  
  return(plt)

}

#' Plot histograms of survival prob. faceted by a given variable.
#'
#' @param survival_data A tibble with survival probabilities in variable named survival.prob.
#' @param colname Character name of variable to facet by.
#' @param nbin Number of bins to use in histogram. Defaults to 30.
#'
#' @return A ggplot2 histogram of probabilites, faceted by the given variable.
#'
#' @examples
#' plots_by(my_data, "sex", 20)
#' plots_by(my_data, "pclass", 15)
plots_by <- function (survival_data, colname, nbin = 30) {
  
  # drop NA values
  survival_data <- survival_data %>% 
    drop_na("survival.prob")
  
  # generate faceted plot
  ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$survival.prob),
      bins = nbin, color = "white"
    ) +
    labs(x = "probability of survival") +
    facet_wrap(colname)
}

# ---- Task 3 ----

# Make predictions with a simple probability cutoff
# Compare predictions to reality
# Get percentage of correct predictions
# Find best cutoff value in range of given values


#' Create a column predicting surival using simple probability cutoff.
#'
#' @param survival_data A tibble with survival probabilities in a variable named survival.prob.
#' @param cutoff_val A probabilility level after which to predict the subject survived. Defaults to 0.51.
#'
#' @return The original tibble with a new variable survival.pred: 1 if survived, 0 if not.
#'
#' @examples
#' survival_predict(my_data)
#' survival_predict(my_data, 0.75)
survival_predict <- function(survival_data, cutoff_val = 0.51) {

  w_cutoff <- survival_data %>% 
    mutate(survival.pred = ifelse(survival.prob > cutoff_val, 1, 0))

  return(w_cutoff)
  
}

#' Generate predictions using cutoff value and check if they were correct.
#'
#' @param survival_data A tibble with survival probabilities in a variable named survival.prob.
#' @param cutoff_val A probabilility level after which to predict the subject survived. Defaults to 0.51.
#'
#' @return See survival_predict, with additional variable pred.correct, 1 if correct else 0.
#'
#' @examples
#' compare_probs_survival(my_data)
#' compare_probs_survival(my_data, 0.75)
compare_probs_survival <- function(survival_data, cutoff_val = 0.51) {
  
  survival_pred <- survival_predict(survival_data, cutoff_val)

  # new value , 1 if survival prediction matches reality else 0
  survival_pred <- survival_pred %>% 
    mutate(pred.correct = ifelse(survival.pred == survived, 1, 0))

  return(survival_pred)
  
}

#' Get a percentage of correct predictions.
#'
#' @param data_set Tibble generated by compare_probs_survival.
#'
#' @return A percentage of correct predictions.
#'
#' @examples
#' precent_survival_correct(my_data)
percent_survival_correct <- function(data_set) {
  
  data_set <- data_set %>% 
    drop_na("pred.correct")

  pcorrect <- 100 * sum(data_set$pred.correct) / nrow(data_set)

  return(pcorrect)
  
}

#' Create plot showing best cutoff value in given range.
#'
#' @param survival_data A tibble with survival probabilities in a variable named survival.prob.
#' @param c.range Atomic vector with lower and upper bounds of cutoff range. Defaults to c(0, 1).
#' @param n.by Increment from lower to upper bound. Defaults to 0.01.
#'
#' @return A ggplot scatter with best cutoff value highlighted.
#'
#' @examples
#' cutoff_compare(my_data, c.range = (0.75, 0.85), n.by = 0.05)
#' cutoff_compare(my_data)
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

