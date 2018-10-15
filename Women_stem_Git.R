#install.packages("dummies")
#library(data.table)
#install.packages("Metrics")
library(Metrics)
library(dplyr)
library(ggplot2)
library(reshape2)
library(dummies)

#fileurl = "data/college-majors/women-stem.csv"
#download.file(fileurl, destfile = "/Users/harishagarwal/Desktop/Harish/Data Science Courses/Git/women_stem.csv")

women_stem = read.csv("/Users/harishagarwal/Desktop/Harish/Data Science Courses/Git/women_stem.csv", header = T)
head(women_stem)
str(women_stem)

# Let's plot a graph to understand the relation between the share of women and the median salary 

women_stem %>% ggplot(aes(x= ShareWomen, y = Median)) + geom_point() + geom_smooth(se= FALSE)

# From the scatter plot, we can conclude that the share of women decreases as the Median salary increases

# Let's look at how major category is distributed across share of women 

women_stem %>% ggplot(aes(x = Major_category, y = ShareWomen)) + geom_bar(stat = "identity")

# Let's look at how major category is distributed across men and women

women_stem1 = women_stem %>% group_by(Major_category) %>% summarise(Men = sum(Men), Women = sum(Women))
women_stem2 = women_stem1 %>% melt(id.vars = "Major_category", measure.vars = c("Men", "Women"), variable.name= "Gender", value.name = "Count")

women_stem2 %>% ggplot(aes(x = Major_category, y = Count, fill = Gender)) + geom_bar(stat = "identity", position = "dodge" ) + theme(axis.text.x = element_text(angle = 90))

# Share of women is more in Biology and Health categories

# Create dummy variables for Major Category

women_stem_dummy = cbind(women_stem, dummy(women_stem$Major_category, sep = "_"))

colnames(women_stem_dummy)[11:15] = c("Bio", "Comp", "Engineering", "Health", "Physical_Sciences")

# Dividing the dataset into training and testing data

set.seed(12345)
smp_size = floor(0.70*nrow(women_stem_dummy))
train_index = sample(seq_len(nrow(women_stem_dummy)), smp_size)
train_data = women_stem_dummy[train_index,]
test_data = women_stem_dummy[-train_index,]

summary(train_data)

# Check the correlation between variables
cor(train_data[-c(1:6)])

# From above EDA, we can see that Major category and median salary impact our dependent variable (women share) significantly
# Fit a linear regression model on these variables

model1 = lm(data = train_data, ShareWomen ~ Women + Median + Bio + Comp + Engineering + Health + Physical_Sciences)

# Review diagnostic measures.
sm1 = summary(model1)
sm1

AIC(model1)
BIC(model1)

# From above diagnostics, we see that the variables Com, Engineering and Physical_Sciences are significant
# Also, the R-squared value is higher than the Adjusted R-squared value
# It means we are including some insignificant variables in our model
# Let's create another model removing a few insignificant variables
# including Median and Bio since they have a good correlation with our dependent variable

model2 = lm(data = train_data, ShareWomen ~ Median + Bio + Comp + Engineering + Health )
sm2 = summary(model2)
sm2

AIC(model2)
BIC(model2)

# Creating another model

model3 = lm(data = train_data, ShareWomen ~ Women + Bio + Comp + Engineering + Health )
sm3 = summary(model3)
sm3

AIC(model3)
BIC(model3)

# The above model is even worse than the previous models

model4 = lm(data = train_data, ShareWomen ~ Bio + Comp + Engineering + Health )
sm4 = summary(model4)
sm4

AIC(model4)
BIC(model4)

# Removing Bio from the model

model5 = lm(data = train_data, ShareWomen ~ Comp + Engineering + Health )
sm5 = summary(model5)
sm5

AIC(model5)
BIC(model5)

# We are good to use model4, since there is not much difference between model4 and model5
# A lower value of AIC and BIC value states that the model is significant.

# Find the root mean squared error for train data
MSE = mean(sm4$residuals^2)
RMSE = sqrt(MSE)
RMSE # 0.105%

mean(train_data$ShareWomen)

# OR

pred_traindata = predict(model4, train_data)
sqrt(Metrics::mse(train_data$ShareWomen, pred_traindata))

# MAPE for train data
mean(abs(pred_traindata - train_data$ShareWomen)/train_data$ShareWomen) # 31.01%

# Predict the test dataset using the model
pred_testdata = predict(model4, test_data)

# RMSE for test data
mse_test = Metrics::mse(test_data$ShareWomen, pred_testdata)
rmse_test = sqrt(mse_test)
rmse_test # 0.109%

# Calculate prediction accuracy and error rates
actuals_pred = as.data.frame(cbind(actuals = test_data$ShareWomen, predicted = pred_testdata))
head(actuals_pred)

cor(test_data$ShareWomen, pred_testdata) # Correlation accuracy = 88.6%

# Now lets calculate the Min Max accuracy and MAPE

min_max_accuracy = mean(apply(actuals_pred, 1, min)/apply(actuals_pred, 1, max))
min_max_accuracy # 80.38%

mape = mean(abs(actuals_pred$predicted - actuals_pred$actuals)/actuals_pred$actuals)
mape

# OR

mean(abs(pred_testdata - test_data$ShareWomen)/test_data$ShareWomen) # MAPE = 26.29%
# The MAPE in test data is less than that in the train data - signifies a good model

# Calculating the R-squared value on test data to check the accuracy of the model

SSE = sum((test_data$ShareWomen - pred_testdata)^2)
SST = sum((test_data$ShareWomen - mean(women_stem$ShareWomen))^2)
Rsquared = 1 - SSE/SST
Rsquared # 0.78 which is very close to that of training data - 0.79

# Plotting some relationships

# install.packages("gridExtra")
library(gridExtra)

train_data_temp = cbind(train_data, pred_traindata)
p1 = train_data_temp %>% ggplot(aes(x = Bio, y = ShareWomen)) + geom_point() + geom_abline()
p2 = train_data_temp %>% ggplot(aes(x = Health, y = ShareWomen)) + geom_point() + geom_abline()
p3 = train_data_temp %>% ggplot(aes(x = Bio, y = pred_traindata)) + geom_point() + geom_abline()
p4 = train_data_temp %>% ggplot(aes(x = Health, y = pred_traindata)) + geom_point() + geom_abline()

grid.arrange(p1, p2, p3, p4, nrow = 2)

test_data$pred = pred_testdata

p5 = test_data %>% ggplot(aes(x = Bio, y = ShareWomen)) + geom_point() + geom_abline()
p6 = test_data %>% ggplot(aes(x = Health, y = ShareWomen)) + geom_point() + geom_abline()
p7 = test_data %>% ggplot(aes(x = Bio, y = pred)) + geom_point() + geom_abline()
p8 = test_data %>% ggplot(aes(x = Health, y = pred)) + geom_point() + geom_abline()

grid.arrange(p5, p6, p7, p8, nrow = 2)
