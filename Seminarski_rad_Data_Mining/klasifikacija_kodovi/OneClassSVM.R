library(e1071)
library(caret)

secomData <- read.csv("~/Downloads/secom_data.csv", header=FALSE, 
                      sep=" ")

secomLabels <- read.csv("~/Downloads/secom_labels_data.csv", header=FALSE, 
                        sep=" ")
#ID column added
secomLabels$ID <- seq.int(nrow(secomLabels)) 

#Remove all columns with variance = 0
secomDataReduced <- secomData[,apply(secomData, 2, var, na.rm=TRUE) != 0]
#ID column added
secomDataReduced$ID <- seq.int(nrow(secomDataReduced)) 


#Every "NaN" value is replaced by median value of particular column
for(i in 1:ncol(secomDataReduced)){
  secomDataReduced[is.na(secomDataReduced[,i]), i] <- median(secomDataReduced[,i], na.rm = TRUE)
}

#data and labels merged
secom <- merge(secomDataReduced, secomLabels, by="ID")

newdata <- mydata[ which(gender=='F' & age > 65),]

secom <- secom[which(secom$V1.y == "-1"),]
# 80% of the whole dataset size is training set
smp_size <- floor(0.8 * nrow(secom))

# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(secom)), size = smp_size)

train <- secom[train_ind, ]
test <- secom[-train_ind, ]

trainX <- train[,1:475]
trainY <- train[,476]

testX <- test[,1:475]
testY <- test[,476]
#One-Class SVM classifier with tuned parameters
model <- svm(trainX,trainY,
               type='one-classification',
               gamma = 0.00001,
               nu=0.99,
               scale=TRUE,
               kernel="radial")

pred <- predict(model, testX)
confusionMatrixTable<-table(Predicted=pred,Reference=testY)
confusionMatrixTable[1:4]
confusionMatrixTable
recall = confusionMatrixTable[1]/(confusionMatrixTable[1]+confusionMatrixTable[2])
#precision = confusionMatrixTable[1]/(confusionMatrixTable[1]+confusionMatrixTable[3])
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionMatrixTable)
#f1 <- 2*(precision*recall)/(precision+recall)
recall


