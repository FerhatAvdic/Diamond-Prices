
library(tidyr)
library(dplyr)
library(ggplot2)
library(caTools)
library(dummies)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# Loading the dataset
df = read.csv('diamonds.csv')
dfp = df
# Number of rows and cols
dim(df)
# Dataframe structure (kinds of values etc.)
str(df)
# General dataframe data, number of unique values, mean etc.
summary(df)
# Sample of the dataframe
head(df, n = 10)

# Count NA values by col
table(is.na(df))

# Show value frequency of categorical cols
par(mfrow=c(1,3))
barplot(table(dfp$cut),main="Cut")
barplot(table(dfp$color),main="Color")
barplot(table(dfp$clarity),main="Clarity")
# # visualize with piechart too
# par(mfrow=c(1,1))
# pie(table(dfp$cut),main="Cut")
# pie(table(dfp$color),main="Color")
# pie(table(dfp$clarity),main="Clarity")
# 
# par(mfrow=c(2,3))
plot(dfp$carat, dfp$price, xlab="Carat Weight", ylab="Price", main="Carat W. vs price", col="blue")
plot(dfp$depth, dfp$price, xlab="depth", ylab="Price", main="depth vs price", col="blue")
plot(dfp$table, dfp$price, xlab="table", ylab="Price", main="table W. vs price", col="blue")
plot(dfp$x, dfp$price, xlab="x", ylab="Price", main="x vs price", col="blue")
plot(dfp$y, dfp$price, xlab="y", ylab="Price", main="y vs price", col="blue")
plot(dfp$z, dfp$price, xlab="z", ylab="Price", main="z vs price", col="blue")

# Encoding cut quality from highest to lowest
dfp$cut = factor(dfp$cut,
                 levels= c("Ideal", "Premium", "Very Good","Good", "Fair"),
                 labels= c(5,4,3,2,1))
# clarity
# How obvious inclusions are within the diamond:
# (in order from best to worst, FL = flawless, I3= level 3 inclusions) 
# FL,IF, VVS1, VVS2, VS1, VS2, SI1, SI2, I1, I2, I3

# Show unique values of clarity
levels(dfp$clarity)
# encode clarity values from highest to lowest
dfp$clarity = factor(dfp$clarity,
                     levels= c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2",  "I1"),
                     labels = c(8,7,6,5,4,3,2,1))
# Create dummy variables
dfp = cbind(dfp, dummy(dfp$color, sep = "_Color_"))
# check new dummies
colnames(dfp)
# Turning categorical values into numerical values
dfp$cut = as.numeric(as.factor(dfp$cut))
dfp$clarity = as.numeric(as.factor(dfp$clarity))
# Show new stats
summary(dfp)
# Split the dataset into train and test set
set.seed(123)
split = sample.split(dfp$price, SplitRatio = 0.75)
train = subset(dfp, split == TRUE)
test = subset(dfp, split == FALSE)
# simple regression
regressor1 = lm(formula = price ~ carat, data= train)
summary(regressor1)
#multiple regression
regressor2 = lm(formula = price ~ . - X - color, data= train)
summary(regressor2)
# eliminate cols with low significance
regressor2 = lm(formula = price ~ . - X - z - color - dfp_Color_J, data= train)
summary(regressor2)
# Predicting the Test set results
testPred1 = predict(regressor1, newdata = test)
RMSE(testPred1, test$price)
testPred2 = predict(regressor2, newdata = test)
RMSE(testPred2, test$price)
# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test$carat, y = test$price), colour = 'red') +
  geom_line(aes(x = test$carat, y = testPred1), colour = 'blue') +
  ggtitle('Price by Carat Weight (Test set)') +
  xlab('Carat Weight') +
  ylab('Price')
#
ggplot() +
  geom_point(aes(x = test$carat, y = test$price), colour = 'red') +
  geom_line(aes(x = test$carat, y = testPred2), colour = 'blue') +
  ggtitle('Price by Carat Weight (Test set)') +
  xlab('Carat Weight') +
  ylab('Price')


dfpMod = within(dfp, rm("X", "dfp_Color_J", "color"))
write.csv(dfpMod, file="Diamonds_Modified.csv")




