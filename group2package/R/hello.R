require(tidyverse)


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

# Task 1

# need to write function to be conducive to all data sets or just this one?
logistic_regression_model_1 <- function(data_set) {

  return(glm(
    formula = survived ~ pclass + sex_binary + age,
    data = data_set,
    family = binomial
  ))
}

# need to have the logistic_regression_model function as an argument here
prob_survival <- function(data_set, pclass, sex_binary, age) {

  # take the outputs of the logistic_regression_model_1 function and calculate the probability of survival
  log_reg_output <- logistic_regression_model_1(data_set)

  log_odds_survival <- (summary(log_reg_output)$coefficients[1,1]) +
    (summary(log_reg_output)$coefficients[2,1] * pclass) +
    (summary(log_reg_output)$coefficients[3,1] * sex_binary)+
    (summary(log_reg_output)$coefficients[4,1] * age)

  # log_odds_survival <- 4.58927-(1.13324*pclass)-(2.49738*sexmale)-(0.03388*age)

    probs_survival <- exp(log_odds_survival) / (1 + (exp(log_odds_survival)))

    return(probs_survival)

    }

# Task 2

survival_stats <- function(data_set) {

  # calculate the odds of survival for every case in a dataset
  new_data <- data_set %>%
    mutate(prob.survival = prob_survival(
      data_set,
      data_set$pclass,
      data_set$sex_binary,
      data_set$age
    ))

  return(new_data)
}

survival_plots <- function(data_set, nbin) {

  #plot probability of survival for each person
  survival_data <- survival_stats(data_set) %>% drop_na("prob.survival")

  ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$prob.survival),
      bins = nbin, color = "white"
    )

}

plots_by_class <- function(data_set, nbin) {

  #plot probability of survival for each person
  survival_data <- survival_stats(data_set) %>% drop_na("prob.survival")

  ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$prob.survival),
      bins = nbin, color = "white"
    ) +
    facet_wrap(~pclass)

}

plots_by_sex <- function(data_set, nbin) {

  #plot probability of survival for each person
  survival_data <- survival_stats(data_set) %>% drop_na("prob.survival")

  ggplot(survival_data) +
    geom_histogram(
      mapping = aes(x = survival_data$prob.survival),
      bins = nbin, color = "white"
    ) +
    facet_wrap(~sex)

}

# Task 3

compare_model_w_reality <- function(data_set, cutoff_val){

  survival_data <- survival_stats(data_set) %>% drop_na("prob.survival")

  survival_data_w_cutoff <- survival_data %>% mutate(survival_cutoff = ifelse(prob.survival > cutoff_val, 1, 0))

  return(survival_data_w_cutoff)

}

# How can we get probs_survival to be plotted for every single social class, and age? And what plot should we use?
