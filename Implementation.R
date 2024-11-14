#------------------------
# Libraries
#------------------------
library(car)
library(caTools)
library(leaps)

#------------------------
# Partitioning
#------------------------
churn_data <- read.csv("Special Topics/Assignment 2/telecom_churn.csv")
head(churn_data)

# Partition the Data using a common split (70% train, 30% test)
set.seed(100)
sample_split <- sample.split(Y = churn_data$Churn, SplitRatio = 0.7)
train_set <- subset(x = churn_data, sample_split == TRUE)
test_set <- subset(x = churn_data, sample_split == FALSE)
head(train_set)
cor(train_set)
scatterplotMatrix(train_set, spread=FALSE, smoother.args=list (lty=2), main="Scatter Plot Matrix")


#------------------------
# Model 1: Multiple Linear Regression using target vs. all predictor variables
#------------------------
fit1 <- lm(Churn ~ AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins, data = train_set)
summary(fit1)

# Regression diagnostic
par(mfrow=c(2,2))
plot(fit1)

line_length_linter(length = 200, line_linter = NULL)

# Multicollinearity
vif(fit1)
sqrt(vif(fit1))

# Outliers
outlierTest(fit1)



#------------------------
# Model 2: Multiple Linear Regression with predictor variables: (ContractRenewal+CustServCalls+DayCalls+RoamMins)
#------------------------
fit2 <- lm(Churn ~ ContractRenewal+CustServCalls+DayCalls+RoamMins, data = train_set)
summary(fit2)

# Regression diagnostic
par(mfrow=c(2,2))
plot(fit2)

# Multicollinearity
vif(fit2)
sqrt(vif(fit2))




#------------------------
# Model 3: Multiple Linear Regression with interactions (DataPlan:DataUsage)
#------------------------
fit3 <- lm(Churn ~ ContractRenewal+CustServCalls+DayCalls+RoamMins+DataPlan:DataUsage, data = train_set)
summary(fit3)

# Regression diagnostic
par(mfrow=c(2,2))
plot(fit3)

# Multicollinearity
vif(fit3)
sqrt(vif(fit3))

# Effects
library(effects)
plot(effect("DataPlan:DataUsage", fit3, ,list(DataUsage=c(2.2, 3.2, 4.2))), multiline=TRUE)
plot(effect("DataPlan:DataUsage", fit3, ,list(DataUsage=c(2.2, 3.2, 4.2))), multiline=FALSE)




#------------------------
# Model 4: Multiple Linear Regression with interactions (MonthlyFee:OverageFee)
#------------------------
fit4 <- lm(Churn ~ MonthlyCharge+OverageFee+MonthlyCharge:OverageFee, data = train_set)
summary(fit4)

# Diagnostic
par(mfrow=c(2,2))
plot(fit4)

# Multicollinearity
vif(fit4)
sqrt(vif(fit4))

# Effects
library(effects)
plot(effect("MonthlyCharge:OverageFee", fit4, ,list(OverageFee=c(2.2, 3.2, 4.2))), multiline=TRUE)
plot(effect("MonthlyCharge:OverageFee", fit4, ,list(OverageFee=c(2.2, 3.2, 4.2))), multiline=FALSE)




#------------------------
# Comparing models
#------------------------

anova(fit2, fit1)

AIC(fit1, fit2, fit3, fit4)

# Metric R-squared
# Interactions
library(leaps)
leaps <- regsubsets(Churn ~ ., data = train_set, nbest = 4)
plot(leaps, scale="adjr2")

# Metric CP
subsets(leaps, statistic="cp", main="CP for all Plots Subsets Regression")
abline(1, 1, lty=2, col="red")

