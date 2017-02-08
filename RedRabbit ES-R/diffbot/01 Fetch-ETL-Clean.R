
library(jsonlite)
library(dplyr)   # distinct
library(stringr)  # str_count
library(elastic)
library(tm)     # text mining package for R
library(e1071)  # package with various machine-learning libraries
library(qdap)   #polarity



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

dict_control <- readRDS('C:/03-MyProjects/Models/dict_control.rds')


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

num=841
mydata2 <- (fromJSON("https://api.diffbot.com/v3/search?token=9ade437e27ca6fbb44e57d07415ce806&num=841&col=GLOBAL-INDEX&query=text%3Azspace%20OR%20text%3Aechopixel%20sortby%3Atimestamp", flatten = TRUE))
mydata2.df <- as.data.frame(mydata2$objects)

#check output
nrow(mydata2.df)
colnames(mydata2.df)

#ETL
# to_be_scored    IEEE 
#######################
t_message <- mydata2.df$text
t_time <- mydata2.df$timestamp
t_urls <- mydata2.df$gburl
t_users <- mydata2.df$author
t_source <- mydata2.df$siteName

sub.df <-  subset(mydata2.df, select = c(docId))
colnames(sub.df) <- c("t_id")

t_sentence <- cleanTweets(t_message)

sub.df$t_message <- t_sentence
sub.df$t_time <- t_time

sub.df$t_urls <- t_urls
sub.df$t_users <- t_users
sub.df$t_label <- "score"
sub.df$t_source <- t_source

#
# DE duplication of rows
nrow(sub.df)

deduplicated.clean.df <- distinct(sub.df, strtrim(t_message, 200), .keep_all = TRUE)
nrow(deduplicated.clean.df)

# remove word size  msg / remove NULL or BLANK MSG
deduplicated.clean.df2 <- subset(deduplicated.clean.df, nchar(t_message) >5) 

newdata <- deduplicated.clean.df[which(nchar(deduplicated.clean.df$t_message) >5), ]
#
# top most important sentences extraction
#nrows <- 
  nrow(deduplicated.clean.df)
  
  texts_short <- strtrim(deduplicated.clean.df$t_message[i],500) 

  
  texts_short <- deduplicated.clean.df$t_summary
  categories <- deduplicated.clean.df$t_label
  
  step1 <- Corpus(VectorSource(texts_short))
  step6  <- cleanCorpus(step1)
  
  ## without this line predict won't work
  dtm_n <- DocumentTermMatrix(step6, control=dict_control)
  
  ## creating data.frame for new data to be scored
  dict_control <- readRDS('C:/03-MyProjects/Models/dict_control.rds')
  model_s <- readRDS('C:/03-MyProjects/Models/model_s.rds')
  model_n <- readRDS('C:/03-MyProjects/Models/model_n.rds')
  model_t <- readRDS('C:/03-MyProjects/Models/model_t.rds')
  df_n <- as.data.frame(inspect(dtm_n))
  
  
  ###################################
  
  df_n$score_svm <- pred_s
  df_n$score_naive <- pred_n
  df_n$score_tree <- pred_t
  
  df_n$t_message <- deduplicated.clean.df$t_message
  df_n$t_summary <- NULL
  df_n$Correct <- NULL
  
  df_n$t_id <- deduplicated.clean.df$t_id
  df_n$t_urls <- deduplicated.clean.df$t_urls
  df_n$t_users <- deduplicated.clean.df$t_users
  
  df_n$t_time <- deduplicated.clean.df$t_time
  
  df_n$t_sme <- 0
  df_n$t_ml <- 0.0
  df_n$t_label <- "score"
  df_n$t_source <- "diffbot echopixel and zspace"
