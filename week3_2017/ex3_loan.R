library(caTools)

# Read in the dataset
loans = read.csv("dati/loans.csv")

str(loans)
summary(loans)

table(loans$not.fully.paid)

library(mice)
set.seed(144)

vars.for.imputation = setdiff(names(loans),"not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
#imputed = read.csv("dati/loans_imputed.csv")

loans[vars.for.imputation] = imputed
summary(loans)

set.seed(144)
split = sample.split(loans$not.fully.paid,SplitRatio = 0.7)

train = subset(loans,split==TRUE)
test = subset(loans,split==FALSE)

loanRegModel <- glm(not.fully.paid ~ .,data=train,family = binomial)
summary(loanRegModel)

# Training set predictions
test$predicted.risk = predict(loanRegModel,newdata = test, type="response")
table(test$not.fully.paid, test$predicted.risk > 0.5)

accuracy = (2401+3)/(2401+3+457+12)
accuracy

accuracy_baseline = (2401+12)/(2401+3+457+12)
accuracy_baseline

# Test set AUC 
library(ROCR)
ROCRpred = prediction(test$predicted.risk,test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

loanRegBivariateModel <- glm(not.fully.paid ~ int.rate,data=train,family = binomial)
summary(loanRegBivariateModel)

# Test set predictions
TestPrediction = predict(loanRegBivariateModel, newdata=test, type="response")
summary(TestPrediction)

table(test$not.fully.paid,TestPrediction > 0.5)

ROCRpred = prediction(TestPrediction,test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

10*exp(0.06*3)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

10*max(test$profit)

most_profitable = subset(test,test$int.rate>=0.15)
mean(most_profitable$profit)
table(most_profitable$not.fully.paid)
110/(327+110)
