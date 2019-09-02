setwd("D:/Study/Data Science/Kaggle/Survival of Titanic mishap")
getwd()

# Loading the library
library("dplyr")          # For data manupulation

# Load the data set
titanic_train = read.csv("train.csv")
titanic_test  = read.csv("test.csv")

titanic = bind_rows(titanic_train, titanic_test)

summary(titanic)
str(titanic)


# Feature Engineering

#***********gsub exersise*********** 
x = "Abhi1245: Where are you heading 2day?"
y = gsub("\\d","***",x)
y = gsub("\\d+","***",x)
rm(x)
rm(y)
#***************************************

# Grab title from the passenager name
titanic$title = gsub("(.*, )|(\\..*)", "", titanic$Name)    # replace the char before , and replace the char after . with the white space
unique(titanic$title)

# title count by sex
table(titanic$Sex, titanic$title)

# merge the rare title to one title
rare_title = c("Capt", "Col", "Don", "Dona", "Jonkheer", "Lady", "Major", "Sir", "Dr", "Rev", "the Countess")

titanic$title[titanic$title == "Mlle"] = "Miss"
titanic$title[titanic$title == "Ms"]   = "Miss"
titanic$title[titanic$title == "Mme"]  = "Mrs"
titanic$title[titanic$title %in% rare_title] = "Rare Title"

table(titanic$Sex, titanic$title)

#***********strsplit exersise***********
x <- "Do you wish you were Mr. Jones?"
strsplit(x, ". ")                               # regular expression
strsplit(x, ". ", fixed = TRUE)                 # actual expression
rm(x)
#***************************************

# Extract surname from the passanger name

# sapply o/p - vector
# Lapply o/p - list
# strsplit outputs a one-item list
titanic$Surname = sapply(titanic$Name, function(x) strsplit(x, split = '[,.]')[[1]][[1]])

length(unique(titanic$Surname))    # 875 unique surname

# Family size of passanger
titanic$Fsize = titanic$SibSp + titanic$Parch + 1   # Family size including himself/herself

#***********paste exersise***********
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9)), sep = ""))
paste("1st", "2nd", "3rd", sep = ", ")
paste0("1st", "2nd", "3rd", sep = ", ")
rm(nth)
#***************************************


# Creating a family variable
titanic$Family = paste(titanic$Surname, titanic$Fsize, sep = '_')

# Visualize the realtionship b/w family and survival i.e. for the train data set
#install.packages("ggthemes")
#install.packages("scales")

library("ggplot2")
library("ggthemes")
library("scales")
ggplot(titanic[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = 'count', position = 'dodge') +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = 'Family Size') #+
#  theme_few()

# Create discretize family size variable
titanic$FsizeD[titanic$Fsize == 1] = 'singleton'
titanic$FsizeD[titanic$Fsize  < 5 & titanic$Fsize > 1] = 'small'
titanic$FsizeD[titanic$Fsize  > 4] = 'large'

table(titanic$FsizeD)
table(titanic$FsizeD, titanic$Survived)

# Family size by survival using mosaic plot
# mosaic plot is used to visualize categorical data which should be in table format
mosaicplot(table(titanic$FsizeD, titanic$Survived), main = 'Family size by Survival', shade =  TRUE)

titanic$Cabin[1:30]

# first char of deck
strsplit(titanic$Cabin[2], NULL)[[1]]

# Creating the deck variable
titanic$Deck = sapply(titanic$Cabin, function(x) strsplit(x,NULL)[[1]][1])
table(titanic$Deck)


# Handling missing values

# Passanger 62 & 830 have missing embarkment value
titanic[c(62,830), 'Embarked']
titanic[c(62,830), 'Fare'][[1]][1]
titanic[c(62,830), 'Pclass']

# Remove missing passengerID rows
embark_fare = titanic %>% filter(PassengerId != 62 & PassengerId != 830)
  
# Visualize embarkment, passenger class & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 80), colour = 'red', linetype = 'dashed', lwd = 2) +
  scale_y_continuous(labels = dollar_format()) #+
 # theme_few()
  
# The median fare of passengers departing from Charbourg almost coincides with $80.
# And passenger 62 & 830 paid $80 and were 1 class passengers, so
# inserting the missing value for embarkment
titanic[c(62,830), "Embarked"]
titanic[c(62,830), "Embarked"] = "C"  
  
summary(titanic)  

# NA value in Fare
which(is.na(titanic$Fare))

titanic[1044,]

# Visualizing the data with embarkment and fare
ggplot(titanic[titanic$Pclass == '3' & titanic$Embarked == 'S',], aes(x = Fare)) +
  geom_density(fill = "#99d6ff", alpha = 0.4) +
  geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = 'red', linetype = "dashed", lwd = 1) +
  scale_x_continuous(labels = dollar_format()) #+
 # theme_few()


titanic$Fare[1044] = median(titanic[titanic$Pclass == '3' & titanic$Embarked == 'S',]$Fare, na.rm = T)
summary(titanic)


# Predictive imputaion
library(mice)

# number of missing age values
sum(is.na(titanic$Age))

# factorize the variables
factor_vars = c("PassengerId", "Pclass", "Sex", "Embarked", "title", "Surname", "Family", "FsizeD")


titanic[factor_vars] = lapply(titanic[factor_vars], function(x) as.factor(x))
str(titanic)

# set a random seed
set.seed(129)
mice_mod = mice(titanic[, !names(titanic) %in% c("PassengerId","Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = 'rf')

mice_output = complete(mice_mod)

# Plotting age distribution
par(mfrow=c(1,2))
hist(titanic$Age, freq = F, main = 'Age:Original Data', col = 'darkgreen', ylim = c(0,0.04))
hist(mice_output$Age, freq = F, main = 'Age: Mice Output', col = 'lightgreen', ylim = c(0,0.04))


# Imputed data looks very similar to the original one, 
# so replace the age variable in the original data with the imputed one

summary(titanic$Age)
titanic$Age = mice_output$Age
summary(titanic$Age)
sum(is.na(titanic$Age))

# Age can be used to do more feature engineering with the data
# Relationship b/w Age and Survived

ggplot(titanic[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram() +
  facet_grid(.~ Sex) +
  theme_few()

# Creating new variable child
titanic$child[titanic$Age < 18] = "child"
titanic$child[titanic$Age >= 18] = "adult"

table(titanic$child, titanic$Survived)

# Creating mother variable
titanic$Mother = "Not Mother"
titanic$Mother[titanic$Sex == "female" & titanic$Age > 18 &
               titanic$Parch > 0 & titanic$title != "Miss"] = "Mother" 

table(titanic$Mother, titanic$Survived)

# Factorize the two variables
titanic$child = factor(titanic$child)
titanic$Mother = factor(titanic$Mother)
str(titanic)

md.pattern(titanic)

library("VIM")
mice_plot = aggr(titanic, numbers = T, sortVars = T, labels = names(titanic))


# Predictions

# Split the data again into training and testing set
train = titanic[1:891,]
test  = titanic[892:1309,]

library("randomForest")
set.seed(754)

rf_model = randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                          Fare + Embarked + title + FsizeD + child + Mother, data = train)

# Plot the error
par(mfrow = c(1,1))
plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

# Variable importance
importance = importance(rf_model)
Varimportance = data.frame(Variables = row.names(importance), Importance = round(importance[,'MeanDecreaseGini'],2))

# Rank variable based on importance
# %>% - To pass the LHS to RHS's first arugument
# dense_rank - rank with no gaps b/w the rank
# mutate - addding new column to the data
rankImportance = Varimportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))

# Visualization of relative importance
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, color = "red") +
  labs(x = "Variables") +
  coord_flip() +
  theme_few()

# Prediction
prediction = predict(rf_model, newdata = test)

solution = data.frame(PassengerId = test$PassengerId, Survived = prediction)

write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
