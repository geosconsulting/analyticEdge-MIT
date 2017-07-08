# Read in the dataset
framingham = read.csv("dati/framingham.csv")

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

framinghamLog <- glm(TenYearCHD ~ ., data = train , family = binomial )
summary(framinghamLog)

predictTest <- predict(framinghamLog,type = "response",newdata = test)
table(test$TenYearCHD,predictTest> 0.5)

#Quanti ne indovina TN+TP/diviso tutte le classificazioni
accuracy <- (1069+11)/(1069+6+187+11)

#Siccome zero è il piu frequante vediamo come predice gli zero
accuracy_baseline <- (1069+6)/(1069+6+187+11)

sensitivity <- 11/(187+11)
specificity <- 1069/(1069+6)  

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
