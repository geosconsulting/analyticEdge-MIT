library(caTools)

# Read in the dataset
parole = read.csv("dati/parole.csv")

table(parole$violator)

summary(parole$state) 
summary(parole$crime)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

summary(parole$state) 
summary(parole$crime)

set.seed(144)
split = sample.split(parole$violator,SplitRatio = 0.7)

train = subset(parole,split==TRUE)
test = subset(parole,split==FALSE)

violatorModel = glm(violator ~ ., data = train, family = binomial)
summary(violatorModel)

violator_example = -4.2411574 + (0.3869904 * 1) + (0.8867192*1) + (-0.0001756*50) +
      (0.4433007*0) + (0.8349797*0) + (-3.3967878*0) + (-0.1238867*3) + (0.0802954*12) + 
      (1.6119919*0) + (0.6837143*1) + (-0.2781054*0) + (-0.0117627*0)
logval <- exp(violator_example)
1/(1+logval)

# Test set predictions
TestPrediction = predict(violatorModel, newdata=test, type="response")
summary(TestPrediction)

table(test$violator, TestPrediction >= 0.5)

specificity <- 167/(167+12)
sensitivity <- 12/(11+12)
accuracy <- (167+12)/(167+12+11+12)
sensitivity
specificity
accuracy


table(test$violator)
179/(179+23)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(TestPrediction, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

