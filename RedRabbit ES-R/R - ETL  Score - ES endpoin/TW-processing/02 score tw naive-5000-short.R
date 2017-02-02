library(e1071)  #predict
library(tm)     # text mining package for R
library(dplyr)
library(qdap)
library(elastic)
library(string)

connect(es_base = "23.101.132.194", es_port = 9220)
connection()

sessionInfo()
setwd("C:/03-MyProjects")
#library(wordnet)

options("scipen"=100, "digits"=4)

lapply(c( 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
       library, character.only = TRUE)   #short

profanity = c(t(read.csv(file = "profanity.csv", header=F)))
#print(prof)

# Number of words
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}
cleanTweets = function(tweets)
{
  tweets_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets)
  tweets_cl = gsub("http[^[:blank:]]+", "", tweets_cl)
  tweets_cl = gsub("@\\w+", "", tweets_cl)
  tweets_cl = gsub("[ \t]{2,}", "", tweets_cl)
  tweets_cl = gsub("^\\s+|\\s+$", "", tweets_cl)
  #tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
  #tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
  tweets_cl <- gsub('\\d+', '', tweets_cl)
  tweets_cl <- iconv(tweets_cl, "UTF-8")
  return(tweets_cl)
}

cleanCorpus <- function(corpus)
{
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp,tolower)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map(corpus.tmp, stemDocument)
  
  corpus.tmp <- tm_map(corpus, removeWords, profanity)
  corpus.tmp <- tm_map(corpus.tmp,removeWords, c(stopwords("english"),"virtual","realiti")) 
  return(corpus.tmp)
}

# to_be_scored
# 
tw_merdan <- Search("tw04-med3d-func",  q="merdan" ,asdf=TRUE, size=10000)
tw_merdan.df <- data.frame(tw_merdan)

tw.df2 <- data.frame(x1_20.df)
tw.df2 <- data.frame(x2_20.df)

#ETL
t_message <- tw.df2$hits.hits._source$message
t.time <- tw.df2$hits.hits._source$'@timestamp'
t.urls <- tw.df2$hits.hits._source$source
t.users <- tw.df2$hits.hits._source$user

sub.df <-  subset(tw.df2, select = c(hits.hits._id))
colnames(sub.df) <- c("t._id")

t.sentence <- cleanTweets(t_message)

sub.df$t_message <- t.sentence
sub.df$t.time <- t.time

sub.df$t.urls <- t.urls
sub.df$t.users <- t.users
sub.df$t.label <- "score"

# DE duplication of rows
#
depublicated.clean.df <- distinct(sub.df, strtrim(sub.df$t_message, 40), .keep_all = TRUE)
#drop non needed column and rows

depublicated.clean.df$`strtrim(t.message, 40)` <- NULL
#
texts_short <- depublicated.clean.df$t_message
categories <- depublicated.clean.df$t.label

step1 <- Corpus(VectorSource(texts_short))
step6  <- cleanCorpus(step1)

## without this line predict won't work
dtm_n <- DocumentTermMatrix(step6, control=dict_control)

## creating data.frame for new data to be scored
df_n <- as.data.frame(inspect(dtm_n))

pred_s <- predict(model_s, df_n, decision.values = TRUE)
pred_n <- predict(model_n, df_n, decision.values = TRUE, probability = TRUE)
pred_t <- predict(model_t, df_n, decision.values = TRUE)
 
###################################
#df_n <- NULL
df_n$score_svm <- pred_s
df_n$score_naive <- pred_n
df_n$score_tree <- pred_t
df_n$t_source <- "tweeter"
df_n$t_message <- depublicated.clean.df$t_message
#df_n$SVM <- NULL

df_n$t_id <- depublicated.clean.df$t._id
df_n$t_urls <- depublicated.clean.df$t.urls
df_n$t_users <- depublicated.clean.df$t.users
df_n$t_time <- depublicated.clean.df$t.time
df_n$t_sme <- 0
df_n$t_ml <- 0.01

########################
# polarity

rt2.text <- as.data.frame(df_n$t_message)

#Add polarity to the rt
pol = 
  lapply(rt2.text, function(txt) {
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
df_n$score_emotion = sapply(pol, function(x) x$all$polarity)

#elastic
names(df_n) <- gsub("\\.", "_", names(df_n))
docs_bulk(df_n, index = "tw-s-1220", type = "tw-s2", doc_ids = df_n$t_id)
docs_bulk(df_n, index = "tw-s-1220b", type = "tw-s2", doc_ids = df_n$t_id)

save.image("C:/03-MyProjects/WebHose/proc0118.RData")


