# Uncomment for installation
#install.packages("RTextTools")
#install.packages("e1071")
library(RTextTools)
library(e1071)

labeledTrainData <- read.table(file = 'files/labeledTrainData.tsv', sep = '\t', header = TRUE)
#trainDataIds <-labeledTrainData[,1]
#trainDataRatings <-labeledTrainData[,2]
#trainDataReviews <-labeledTrainData[,3]


labeledTrainData[1, 'review']


positiveData <- data.frame()
for (i in 1:nrow(labeledTrainData))
{
  if(labeledTrainData[i, 'sentiment'] == 1)
  {
    positiveData[i,1] <- labeledTrainData[i, 'review']
  }
}

positiveData <- as.matrix(positiveData[!is.na(positiveData)])
