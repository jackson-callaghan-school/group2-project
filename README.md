# group2-project

 a project, made by group 2

 sibsp is number of siblings/spouses abroad
 parch is number of parents/children abroad

Information taken from: http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets

Guide to variable names:
http://campus.lakeforest.edu/frank/FILES/MLFfiles/Bio150/Titanic/TitanicMETA.pdf

## Todo:

Task 1:

- write a function which fits a logistic regression model
  
  - takes arguments of data, and names of variables to fit
  
  - returns model

- write function to calculate probability of survival for a given set of inputs using given model
  
  - takes arguments of variables for one person
  
  - returns probability of survival



Task 2:

- write function to create tibble of all probabilities of survival (jackson)
  
  - takes arguments of tibble of all data
  
  - uses existing functions to make model and calculate probabilities
  
  - returns a tibble with probabilities added

- write function to output graph(s) of survival probability
  
  - takes arguments of tibble of all data
  
  - uses existing functions
  
  - makes plots



Task 3:

- write function which guesses at survival

  - takes probability cutoff value
  
  - returns df with new col of 0 or 1 for predicted survival
  
- write function which compares guess of survival to predicted survival

  - output df with new column for correct prediction or not
  
- analyze/visualize accuracy of model

  - graph of % correct vs cutoff value?
  
  - graph of all survival probabilities colored by correct or not?
  
  - something else?
