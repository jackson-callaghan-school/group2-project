# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#Task 1

logistic_regression_model_1 <- function(data_set){
  
  data_set <- titanic_data     
  glm(formula = survived ~ pclass + sex + age, data = data_set, family = binomial) #need to write function to be conducive to all data sets or just this one?  
}

prob_survival <- function(pclass, sexmale, age){ 
  
  #take the outputs of the logistic_regression_model_1 function and calculate the probability of survival
  
  log_odds_survival <- 4.58927-(1.13324*pclass)-(2.49738*sexmale)-(0.03388*age)
  probs_survival <- exp(log_odds_survival)/(1+(exp(log_odds_survival)))
  
  return(probs_survival)
}

#Task 2

# How can we get probs_survival to be plotted for every single social class, and age? And what plot should we use? 


