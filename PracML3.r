setwd("C:\\Users\\Minerva\\Dropbox\\Desk2015\\Rwork\\MachineLearn")

#Read in training data
trainUrl="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training = "pml-training.csv"
trainingF <- read.csv(training, na.strings=c("NA",""), header=TRUE)
colnames_train <- colnames(trainingF)


#Test data

testUrl = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testing="pml-testing.csv"
testingF <- read.csv(testing, na.strings=c("NA",""), header=TRUE)
colnames_test <- colnames(testingF)


################
# Count the number of non-NAs in each col.
nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

# Build vector of missing data or NA columns to drop.
colcnts <- nonNAs(trainingF)
drops <- c()
for (cnt in 1:length(colcnts)) {
  if (colcnts[cnt] < nrow(trainingF)) {
    drops <- c(drops, colnames_train[cnt])
  }
}

# Drop NA data and the first 7 columns as they're unnecessary for predicting.
trainingF <- trainingF[,!(names(trainingF) %in% drops)]
trainingF <- trainingF[,8:length(colnames(trainingF))]

testingF <- testingF[,!(names(testingF) %in% drops)]
testingF <- testingF[,8:length(colnames(testingF))]

#########Model fit

library(caret)

fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
##10 cross cv


fit <- train(classe~., method="rf",data=trainingF, trControl = fitControl) #fit random forest to the training model

fit$results #training model has 99% accuracy

prediction <- predict(fit, testingF)
prediction

