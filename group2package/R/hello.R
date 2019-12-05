

# read data
read_data <- function (filename) {
  # read the data and transform to prep for logistic regression
  titanic_data <- read.csv("titanic_group_project.csv")
  titanic_data$sex <- as.character(titanic_data$sex)
  titanic_data <- titanic_data %>% mutate(sex_binary = ifelse(sex == "female", 1,0))

  return(titanic_data)
}

#Task 1

logistic_regression_model_1 <- function(data_set){

  data_set <- titanic_data
  glm(formula = survived ~ pclass + sex + age, data = data_set, family = binomial) #need to write function to be conducive to all data sets or just this one?
}

prob_survival <- function(pclass, sexmale, age){

  #take the outputs of the logistic_regression_model_1 function and calculate the probability of survival
  # has to call logistic regression

  log_odds_survival <- 4.58927-(1.13324*pclass)-(2.49738*sexmale)-(0.03388*age)
  probs_survival <- exp(log_odds_survival)/(1+(exp(log_odds_survival)))

  return(probs_survival)
}

#Task 2

survival_stats <- function (data_set) {

  # calculate the odds of survival for every case in a dataset
  new_data <- data_set %>%
    mutate(prob.survival = prob_survival(
      data_set$pclass,
      data_set$sex,
      data_set$age
    ))

  return(new_data)
}

survival_plots <- function (data_set) {

  #plot probability of survival for each person
  survival_data <- survival_stats(data_set)

  ggplot(survival_data) +
    geom_histogram(mapping = aes(x = survival_data$prob.survival))

}

# How can we get probs_survival to be plotted for every single social class, and age? And what plot should we use?


