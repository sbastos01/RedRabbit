library(RTextTools)
library(e1071)  #predict
library(caret)
library(tm)     # text mining package for R
library(e1071)  # package with various machine-learning libraries
library(party)
library(rminer)
library(dplyr)
library(SnowballC)
library("ggplot2")
library(qdap)
library(elastic)
library(string)
library(monkeylearn)
library(data.table)
library(stringr)  #
library(LSAfun)

sessionInfo()
setwd("C:/03-MyProjects")
#library(wordnet)

options("scipen"=100, "digits"=4)

lapply(c( 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
       library, character.only = TRUE)

#to be scored
tw.df2 <- data.frame(x1_01.df)

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
# scoring

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

# what time was in in New-York
#sub.df$t.time.ny <-   ymd_hms(t.time, tz="America/New_York")

sub.df$t.urls <- t.urls
sub.df$t.users <- t.users
sub.df$t.label <- "score"

#
# DE duplication of rows
#
depublicated.clean.df <- distinct(sub.df, strtrim(sub.df$t_message, 40), .keep_all = TRUE)

#drop non needed column and rows
#
depublicated.clean.df$`strtrim(t.message, 40)` <- NULL
#
# remove word size  msg / remove NULL or BLANK MSG
#depublicated.clean.df <- subset(depublicated.clean.df, nchar(sub.df$t_message) >5 & nwords(sub.df$t_message) >1) 

#
texts_short <- depublicated.clean.df$t_message
categories <- depublicated.clean.df$t.label

step1 <- Corpus(VectorSource(texts_short))
step6  <- cleanCorpus(step1)

# >>>> USED   >>> setp6
#dict_control  <- list(dictionary=names(df))

## without this line predict won't work
dtm_n <- DocumentTermMatrix(step6, control=dict_control)

## creating data.frame for new data to be scored
df_n <- as.data.frame(inspect(dtm_n))

pred_s <- predict(model_s, df_n, decision.values = TRUE)
pred_n <- predict(model_n, df_n, decision.values = TRUE, probability = TRUE)
pred_t <- predict(model_t, df_n, decision.values = TRUE)
 
###################################

df_n$score_SVM <- pred_s
df_n$score_naive <- pred_n
df_n$score_tree <- pred_t
df_n$t_souce <- "tweeter"
df_n$t_message <- depublicated.clean.df$t_message
df_n$Correct <- NULL

df_n$t_id <- depublicated.clean.df$t._id
df_n$t_urls <- depublicated.clean.df$t.urls
df_n$t_users <- depublicated.clean.df$t.users
df_n$t_time <- depublicated.clean.df$t.time

#df_n$t_organizations <- depublicated.clean.df$t_organizations
#df_n$t_persons <- depublicated.clean.df$t_persons
#df_n$t_locations <- depublicated.clean.df$t_locations


########################
# polarity
#
rt2.text <- as.data.frame(df_n$t_message)
#rt$receiver <- el$receiver

#Add polarity to the rt
pol = 
  lapply(rt2.text, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
df_n$score_emotion = sapply(pol, function(x) x$all$polarity)
# As reality check, what are the most and least positive articles
wh$t_message[which.min(df_n$score_emotion)]

only_seeded_df <- subset(df_n, t_label !=  score_naive & t_label != "seeded")
#only_seeded_all <- rbind(only_seeded_df, only_seeded_df02)
View(only_seeded_df)

save.image("C:/03-MyProjects/score08.RData")

#elastic
connect(es_base = "23.101.132.194", es_port = 9220)
connection()
count(index='wh-s-0104', type='wh-s') 
#index_delete("tw-clean-de-dup")
#
names(df_n) <- gsub("\\.", "_", names(df_n))
docs_bulk(df_n, index = "wh-s-0104", type = "wh-s", doc_ids = df_n$t_id)
count('wh-s-0104')
index_delete("plos")
aliases_get() 
