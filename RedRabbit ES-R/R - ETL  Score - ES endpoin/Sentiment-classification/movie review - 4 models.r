library(tm)
library(wordcloud)
library(SnowballC)
library(caret)
library(e1071)
library(rminer)
library(party)
library(kernlab)

install.packages("e1071")
install.packages("rminer")
install.packages("kernlab")
install.packages("party")
install.packages("caret")

# reove all list
rm(list=ls(all=T))

setwd("C:/03-MyProjects/Sentiment-classification/")
FileName="Movie1.csv"
movie_t_messages = read.csv(FileName, header= T, stringsAsFactors = F)
View(movie_t_messages)

movie_t_messages$Label <-factor(movie_t_messages$Label)
#movie_t_messages$t_message <- lapply(movie_t_messages$t_message, iconv, from="ISO-8859-15", to="UTF-8")

movie_t_messages$t_message[1]
nrow(movie_t_messages)
prop.table(table(movie_t_messages$Label))
set.seed(100)
inTrain <-createDataPartition(y=movie_t_messages$Label,p=0.5, list=FALSE)
train <- movie_t_messages[inTrain,]
testdata <- movie_t_messages[-inTrain,]

inTest <- createDataPartition(y=testdata$Label,p=0.5, list=FALSE)
test1 <-testdata[inTest,]
test2 <-testdata[-inTest,]

prop.table(table(test1$Label))
prop.table(table(test2$Label))
prop.table(table(testdata$Label))

View(test1)

options(mc.cores=1)
train_corpus <- Corpus(VectorSource(train$Label))
length(train_corpus)

cleanCorpus <- function(corpus)
  {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp,tolower)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map(corpus.tmp, stemDocument)
  corpus.tmp <- tm_map(corpus.tmp,removeWords, stopwords("english"))
  return(corpus.tmp)
}
  step6  <- cleanCorpus(train_corpus)
   inspect(step6[1:5])

   train_dtm_bin <- DocumentTermMatrix(step6, control=list(weighting = weightBin))
   dim(train_dtm_bin)
 str(train_dtm_bin)
inspect(train_dtm_bin[1,1:8])

dtm.as.matrix <- as.matrix(train_dtm_bin)
View(dtm.as.matrix)

train_rmsp_bin_95 = removeSparseTerms(train_dtm_bin, 0.97)
dim(train_rmsp_bin_95)

train_BOW_95 <- as.matrix(train_rmsp_bin_95)
View(train_BOW_95)

mean_train_95 = sort(colMeans(train_BOW_95), decreasing=T)
average_top20_train <- mean(mean_train_95[1:20])
average_top20_train
mean_train_95[1]
barplot(mean_train_95[1:10], border=NA,las= 3,xlab="Top 20",ylab="Av Binary Weightd", ylim= c(0,1.5))

train_data_bin_95 = data.frame(y=train$Label, train_BOW_95)
View(train_data_bin_95)

###### TEST 1
test1_corpus <- Corpus(DataframeSource(as.matrix(test1$Label)))

test1_step6  <- cleanCorpus(test1_corpus)
inspect(test1_step6[1:5])

#create weight
#test1_dtm_bin_95 <- DocumentTermMatrix(test1_step6, list(weighting = weightBin, dictionary = train_BOW_95))

test1_dtm_bin_95 <- DocumentTermMatrix(test1_step6, list(weighting = weightBin))
 test1_m_bin_95 <-as.matrix(test1_dtm_bin_95)
View(test1_m_bin_95)

#combine LABEL
test1_data_bin_95 = data.frame(y=test1$Label,test1_m_bin_95)

#plot
mean_test1_95 = sort(colMeans(test1_m_bin_95), decreasing=T)
average_top20_test1 <- mean(mean_test1_95[1:20])
average_top20_test1
mean_test1_95[1]
barplot(mean_test1_95[1:20], border=NA,las= 3,xlab="Top 20",ylab="Test1 Av Binary Weightd", ylim= c(0,1.5))

###### TEST 2
#Adding Interest  data$size      <- 0 

test2_corpus <- Corpus(DataframeSource(as.matrix(test2$Label)))
test2_step6  <- cleanCorpus(test2_corpus)
inspect(test2_step6[1:5])

#create weight
# error
# test2_dtm_bin_95 <- DocumentTermMatrix(test2_step6, list(weighting = weightBin, dictionary = train_BOW_95))

test2_dtm_bin_95 <- DocumentTermMatrix(test2_step6, list(weighting = weightBin))
test2_m_bin_95 <-as.matrix(test2_dtm_bin_95)
#add int
View(test2_m_bin_95)

#
#combine LABEL and new column for "interest"
#drop column -- test2_m_bin_95<-test2_m_bin_95[,-8]
#test2_m_bin_95<-cbind(test2_m_bin_95, test2_m_bin_95[,"ola"])

test2_m_bin_95<-cbind(test2_m_bin_95, "0")
colnames(test2_m_bin_95)[7:7]<-c("interest")
View(test2_m_bin_95)

test2_data_bin_95 = data.frame(y=test2$Label,test2_m_bin_95)

#plot
mean_test2_95 = sort(colMeans(test2_m_bin_95), decreasing=T)
average_top20_test2 <- mean(mean_test2_95[1:20])
average_top20_test2
mean_test2_95[1]
barplot(mean_test2_95[1:20], border=NA,las= 3,xlab="Top 20",ylab="test2 Av Binary Weightd", ylim= c(0,1.5))


View(train_data_bin_95)
#ctree 
BOW_ctree <- ctree(y ~ ., data=train_data_bin_95)

#test1
test1Pred = predict(BOW_ctree, newdata = test1_data_bin_95)
# error
# confusionMatrix(test1Pred, test1_data_bin_95[,1], positive = "Positive", dnn=c("Prediction", "True"))
              
# Accuracy 
conf.mat <- table("Predictions" =test1Pred, Actual= test1_data_bin_95[,1])
conf.mat
(accuracy <- sum(diag(conf.mat)) / length(inTest)* 100)

mmetric(test1Pred, test1_data_bin_95[,1],c("ACC","TPR","PRECISION","F1"))

################test2
#add interest column
#View(test2_data_bin_95)
# droping columns to match train 
# test2_data_bin_95<-test2_data_bin_95[,-4]
# test2Pred = predict(BOW_ctree, newdata = test2_data_bin_95)
# confusionMatrix(test2Pred, test2_data_bin_95[,1], positive = "Positive", dnn=c("Prediction", "True"))
# Accuracy  conf.mat <- table("Predictions" =test2Pred, Actual= test2_data_bin_95[,1])
# conf.mat
#(accuracy <- sum(diag(conf.mat)) / length(-inTest)* 100)

#############        naive

BOW_nB <-naiveBayes(y ~., data=train_data_bin_95)

#test1
test1Pred = predict(BOW_nB, newdata = test1_data_bin_95)
# error
# confusionMatrix(test1Pred, test1_data_bin_95[,1], positive = "Positive", dnn=c("Prediction", "True"))

# Accuracy 
conf.mat <- table("Predictions" =test1Pred, Actual= test1_data_bin_95[,1])
conf.mat
(accuracy <- sum(diag(conf.mat)) / length(inTest)* 100)

mmetric(test1Pred, test1_data_bin_95[,1],c("ACC","TPR","PRECISION","F1"))

#test2
test2Pred = predict(BOW_nB, newdata = test2_data_bin_95)
# Accuracy 
conf.mat <- table("Predictions" =test2Pred, Actual= test2_data_bin_95[,1])
conf.mat
(accuracy <- sum(diag(conf.mat)) / length(inTest)* 100)

mmetric(test2Pred, test2_data_bin_95[,1],c("ACC","TPR","PRECISION","F1"))

################### SVM

BOW_ksvm <- ksvm(y~., data=train_data_bin_95)

#test1
test1Pred = predict(BOW_ksvm, newdata = test1_data_bin_95)
# error
# confusionMatrix(test1Pred, test1_data_bin_95[,1], positive = "Positive", dnn=c("Prediction", "True"))

# Accuracy 
conf.mat <- table("Predictions" =test1Pred, Actual= test1_data_bin_95[,1])
conf.mat
(accuracy <- sum(diag(conf.mat)) / length(inTest)* 100)

mmetric(test1Pred, test1_data_bin_95[,1],c("ACC","TPR","PRECISION","F1"))

#test2
test2Pred = predict(BOW_ksvm, newdata = test2_data_bin_95)
# Accuracy 
conf.mat <- table("Predictions" =test2Pred, Actual= test2_data_bin_95[,1])
conf.mat
(accuracy <- sum(diag(conf.mat)) / length(inTest)* 100)

mmetric(test2Pred, test2_data_bin_95[,1],c("ACC","TPR","PRECISION","F1"))
