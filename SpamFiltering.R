rm(list=ls())

sms = read.csv("spamraw.csv")
View(sms)
str(sms)
sms$text = as.character(sms$text)
str(sms)
table(sms$type)

library(tm)
sms_corpus <- VCorpus(VectorSource(sms$text))
sms_corpus
#To receive a summary of specific messages we make use of insapect() function
inspect(sms_corpus[1:2])
#To view actual message text we use this
as.character(sms_corpus[[1]]) #Double bracket is must
#to view multiple messages
lapply(sms_corpus[1:2],as.character)

sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower)) #converting to lower case letters
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers) #removing numbers
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords()) #remvoing stop words
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation) #remving punctuation
#to work around the default behavior of remove punctuations, simply create a function i.e.,
#replacePunctuations <- function(x){
#gsub("[:punct:]",+," ",*)
#}

#Now lets know what is stemming
library(SnowballC)
#single word stemming
wordStem(c("learn","learned","learning"))
#Doing stemming to entire corpus 
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)#removing spaces after doing above process

#final step is to split the messages into individual components through process called Tokenization
#In this case tokens are words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#What happened here??
#Ans:-DocumentTermMatrix function will take a corpus and create a data structure called DTM in which,
#Rows indicate Documents(sms messages) and column indicates Terms(words)

sms_dtm

#sms_dtm2 <- DocumentTermMatrix(sms_corpus,
#                               control = list(tolower = TRUE,
#                                              removeNumbers = TRUE,
#                                              stopwords = TRUE,
#                                              removePunctuatio = TRUE,
#                                              stemming = TRUE))
#Difference in doing step-by-step and single shot is,we can see slight change in no. of terms in matrix.

sms_dtm1 <- removeSparseTerms(sms_dtm, 0.999)
sms_dtm1

#Maximal term length is the biggest number of characters of one (or more) of your terms in the document term matrix.

require(caTools)
set.seed(101)
dataset <- as.data.frame(as.matrix(sms_dtm1))
dataset$classification <- sms$type
split <- sample.split(dataset, SplitRatio = .75)
train <- subset(dataset, split == TRUE)
test  <- subset(dataset, split == FALSE)
#lets check whether the subsets are representing complete set of sms data
prop.table(table(train$classification))
prop.table(table(test$classification))

#install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE)

#let's visualise spam and ham messages
spam <- subset(sms,type == "spam")
ham <- subset(sms,type == "ham")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5)) #max.words is most common words
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

convert_counts <- function(x){
  x <- ifelse(x>0,"Yes","No") 
}
sms_train <- as.data.frame(apply(train[-1189], MARGIN = 2, convert_counts)) 
sms_test <- as.data.frame(apply(test[-1189], MARGIN = 2, convert_counts)) 
library(e1071)
sms_classifier <- naiveBayes(x = sms_train, y = train$classification)
sms_test_pred <- predict(sms_classifier,sms_test)
library(gmodels)
cm <- table(sms_test_pred,test$classification)
accuracy <- sum(diag(cm))/sum(cm)
accuracy
