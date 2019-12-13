# group2-project

 a project, made by group 2

 sibsp is number of siblings/spouses abroad
 parch is number of parents/children abroad

Information taken from: http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets

Guide to variable names:
http://campus.lakeforest.edu/frank/FILES/MLFfiles/Bio150/Titanic/TitanicMETA.pdf

What we did:

For this project, our group took a data set containing information on those who were on the Titanic before the historical incident. We wanted to look at how each person's probability of survival coulld be predicted based on his/her social class, sex, and age, as people are commonly predisposed to have higher probabilities of survival if they are first class, female, and/or young. We divided this project up into three tasks, and this helped us organize the functions that we wrote. 

We allow the user to fit a logistic regression model to the data set, with survival as a binary outcome. We then allow for the user to calculate the probability of an individual with a given social class, sex, and age. The user can then create a tibble with the probabilities of survival for each  person in the data set and plot these probabilities. We then generalize these capabilities and assess the accuracy of our logistic regression model by comparing the probabilities of survival calculated by the logistic regression model (based on a cutoff value that the user inputs) and what happened in reality. Finally, we plot the percentage of correct survival guesses by the regression model and plot those values against the possible cutoff values that the user could've inputted. 

