#https://rpubs.com/cen0te/naivebayes-sentimentpolarity

install.packages("tm")
install.packages("stringr")
install.packages("caret")

library(tm)  # text mining and document management
library(stringr)  # character manipulation with regular expressions
library(e1071)
library(dplyr)
library(caret)


df <- read.csv("files/movie-pang02.csv", stringsAsFactors = FALSE)
glimpse(df)

#specify RNG seed
set.seed(1)

#randomize DF
df <- df[sample(nrow(df)), ]

glimpse(df)

#convert to factors (0 and 1) not needed when train data already in 0 1
df$class <- as.factor(df$class)

#Initiate Corpus/Corpora collection from vector source
corpus <- Corpus(VectorSource(df$text))

#Clean the Corpus collection
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

#Initiate a term-document matrix
dtm <- DocumentTermMatrix(corpus.clean)

inspect(dtm[40:50, 10:15])

#initiate train and test data from datafram (df) data
df.train <- df[1:1750,]
df.test <- df[1751:2000,]

#Initiate train and test data from documenttermmatrix (dtm) data
dtm.train <- dtm[1:1750,]
dtm.test <- dtm[1751:2000,]

corpus.clean.train <- corpus.clean[1:1750]
corpus.clean.test <- corpus.clean[1751:2000]

dim(dtm.train)


minfreq <- findFreqTerms(dtm.train, 10)
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


system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )

#saveRDS(classifier, "./classifier.rds")
#classifierSaved<- readRDS("./classifier.rds")

system.time( prediction <- predict(classifier, newdata=testNB) )
length(prediction)


saveRDS(prediction, "./prediction.rds")

predictionSaved<- readRDS("./prediction.rds")


table("Predictions"= prediction,  "Actual" = df.test$class )

conf.mat <- confusionMatrix(prediction, df.test$class)

conf.mat
