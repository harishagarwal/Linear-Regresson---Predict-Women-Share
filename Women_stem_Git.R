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

str(women_stem)

# Let's plot a graph to understand the relation between the share of women and the median salary 

women_stem %>% ggplot(aes(x= ShareWomen, y = Median)) + geom_point() + geom_smooth(se= FALSE)

# From the scatter plot, we can conclude that the share of women decreases as the Median salary increases

# Let's look at how major category is distributed across men and women

# women_stem %>% ggplot(aes(x = Major_category, y = ShareWomen)) + geom_bar(stat = "identity")

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

# From above EDA, we can see that Major category and median salary impact our dependent variable (women share) significantly
# Fit a linear regression model on these variables

model = lm(data = train_data, ShareWomen ~ Women + Median + Bio + Comp + Engineering + Health + Physical_Sciences)

# Review diagnostic measures.
sm = summary(model)
sm
AIC(model)
BIC(model)

# A lower value of AIC and BIC value states that the model is significant.

# Find the mean squared error for train data
mean(sm$residuals^2)

# OR

pred_traindata = predict(model, train_data)
Metrics::mse(train_data$ShareWomen, pred_traindata) # 1.04%

# MAPE for train data
mean(abs(pred_traindata - train_data$ShareWomen)/train_data$ShareWomen) # 29.28%

# Predict the test dataset using the model
pred_testdata = predict(model, test_data)

# MSE for test data
Metrics::mse(test_data$ShareWomen, pred_testdata) # 1.24%

# Calculate prediction accuracy and error rates
actuals_pred = as.data.frame(cbind(actuals = test_data$ShareWomen, predicted = pred_testdata))
head(actuals_pred)

cor(test_data$ShareWomen, pred_testdata) # Correlation accuracy = 87.9%

# Now lets calculate the Min Max accuracy and MAPE

min_max_accuracy = mean(apply(actuals_pred, 1, min)/apply(actuals_pred, 1, max))
min_max_accuracy # 79.51%

mape = mean(abs(actuals_pred$predicted - actuals_pred$actuals)/actuals_pred$actuals)
mape

# OR

mean(abs(pred_testdata - test_data$ShareWomen)/test_data$ShareWomen) # MAPE = 27.01%

# Plotting some relationships

par(mfrow = c(2,2))

train_data_temp = cbind(train_data, pred_traindata)
train_data_temp %>% ggplot(aes(x = Women, y = ShareWomen)) + geom_point() + geom_abline()
train_data_temp %>% ggplot(aes(x = Median, y = ShareWomen)) + geom_point() + geom_abline()
train_data_temp %>% ggplot(aes(x = Women, y = pred_traindata)) + geom_point() + geom_abline()
train_data_temp %>% ggplot(aes(x = Median, y = pred_traindata)) + geom_point() + geom_abline()


test_data$pred = pred_testdata

test_data %>% ggplot(aes(x = Women, y = ShareWomen)) + geom_point() + geom_abline()
test_data %>% ggplot(aes(x = Median, y = ShareWomen)) + geom_point() + geom_abline()
test_data %>% ggplot(aes(x = Women, y = pred)) + geom_point() + geom_abline()
test_data %>% ggplot(aes(x = Median, y = pred)) + geom_point() + geom_abline()

