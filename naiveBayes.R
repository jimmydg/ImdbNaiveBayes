#uncomment to install libraries
#install.packages("tm")
#install.packages("RTextTools")
#install.packages("e1071")
#install.packages("dplyr")
#install.packages("caret")

#load libraries
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

#read data and skip the first column
labeledTrainData <- read.table(file = 'files/labeledTrainData.tsv', sep = '\t', header = TRUE, colClasses = c('NULL', 'factor', 'character'))

#Randomize data
set.seed(1)
labeledTrainData <- labeledTrainData[sample(nrow(labeledTrainData)), ]
labeledTrainData <- labeledTrainData[sample(nrow(labeledTrainData)), ]

#glimpse(labeledTrainData)

#Initiate Corpus collection from source text
corpus <- Corpus(VectorSource(labeledTrainData$review))

#corpus[[143]]$content

#Corpus clean function
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

#Initiate a document-term matrix
dtm <- DocumentTermMatrix(corpus.clean)

#retrieve information from DTM
#inspect(dtm[40:50, 10:15])

#Train en testdata aanmaken van de originele data
labeledTrainData.train <- labeledTrainData[1:1500, ]
labeledTrainData.test <- labeledTrainData[1501:2000, ]


#Train en testdata aanmaken van de dtm
dtm.train <- dtm[1:1500, ]
dtm.test <- dtm[1501:2000, ]

#Data opschonen
#minfreq niet gebruikt om hogere accuracy te behalen
corpus.clean.train <- corpus.clean[1:1500]
corpus.clean.test <- corpus.clean[1501:2000]

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train)
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test)

#dim(dtm.train.nb)
#dim(dtm.test.nb)

#Function to replace 0 and 1's by boolean values (binarized Naive-bayes)
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

#initiate nb classifier
system.time(
  classifier <- naiveBayes(trainNB, labeledTrainData.train$sentiment, laplace = 1)
  )

saveRDS(classifier, "./classifier.rds")
#classifierSaved<- readRDS("./classifier.rds")

#initiatie prediction
system.time(
  prediction <- predict(classifier, newdata=as.matrix(testNB))
  )

#length(prediction)

saveRDS(prediction, "./prediction.rds")

#predictionSaved<- readRDS("./prediction.rds")


table("Predictions"= prediction,  "Actual" = labeledTrainData.test$sentiment )

conf.mat <- confusionMatrix(prediction, labeledTrainData.test$sentiment)

conf.mat

print(paste('accuracy: ', conf.mat$overall['Accuracy']))