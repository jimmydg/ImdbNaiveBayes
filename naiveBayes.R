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

glimpse(df)

#Initiate Corpus collection from source text
corpus <- Corpus(VectorSource(labeledTrainData$review))

#Corpus clean function
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

#Initiate a term-document matrix
dtm <- DocumentTermMatrix(corpus.clean)

#retrieve information from corpus
inspect(dtm[40:50, 10:15])

#initiate train and test data from datafram (df) data
labeledTrainData.train <- labeledTrainData[1:1750,]
labeledTrainData.test <- labeledTrainData[1751:2000,]

#Initiate train and test data from documenttermmatrix (dtm) data
dtm.train <- dtm[1:1750,]
dtm.test <- dtm[1751:2000,]

#clean the data
corpus.clean.train <- corpus.clean[1:1750]
corpus.clean.test <- corpus.clean[1751:2000]

dim(dtm.train)


minfreq <- findFreqTerms(dtm.train, 100)
length((minfreq))


dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = minfreq))

dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = minfreq))
dim(dtm.test.nb)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

#get time spent calculating and other info
system.time(
  classifier <- naiveBayes(trainNB, labeledTrainData.train$sentiment, laplace = 1)
  )

saveRDS(classifier, "./classifier.rds")
#classifierSaved<- readRDS("./classifier.rds")

system.time( prediction <- predict(classifier, newdata=testNB) )
length(prediction)


saveRDS(prediction, "./prediction.rds")

predictionSaved<- readRDS("./prediction.rds")


table("Predictions"= prediction,  "Actual" = labeledTrainData.test$sentiment )

conf.mat <- confusionMatrix(prediction, labeledTrainData.test$sentiment)

conf.mat