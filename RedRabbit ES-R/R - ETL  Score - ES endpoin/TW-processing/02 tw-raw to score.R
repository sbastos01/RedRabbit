library(e1071)  #predict
library(tm)     # text mining package for R
library(dplyr)   # distinct
library(qdap)   #polarity
library(elastic) #
library("httr") # if verbose in elastic

connect(es_base = "23.101.132.194", es_port = 9220)
connection()

sessionInfo()
setwd("C:/03-MyProjects")
options("scipen"=100, "digits"=4)
options(stringsAsFactors = FALSE)

profanity = c(t(read.csv(file = "profanity.csv", header=F)))

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
scored_sequence <- "01-28"
tobe_scored <- paste0("tw-raw-" , scored_sequence)

  tw <- Search(tobe_scored,  q="*" ,asdf=TRUE, size=10000)
  tw.df2 <- NULL
  tw.df2 <- as.data.frame(tw)

#ETL
t_message <- tw.df2$hits.hits._source$t_message
t_time <- tw.df2$hits.hits._source$t_time
t_urls <- tw.df2$hits.hits._source$t_urls
t_users <- tw.df2$hits.hits._source$t_users
t_id <- tw.df2$hits.hits._source$t_id

sub.df <- NULL
sub.df <-  subset(tw.df2, select = c(hits.hits._id))
colnames(sub.df) <- c("t_id")

t_sentence <- cleanTweets(t_message)
sub.df$t_message <- t_sentence
sub.df$t_id <- t_id
sub.df$t_time <- t_time
sub.df$t_urls <- t_urls
sub.df$t_users <- t_users
sub.df$t_label <- "score"

# DE duplication of rows
#
dedublicated.clean.df <- NULL
dedublicated.clean.df <- distinct(sub.df, strtrim(sub.df$t_message, 40), .keep_all = TRUE)
dedublicated.clean.df$`strtrim(t.message, 40)` <- NULL
#
texts_short <- dedublicated.clean.df$t_message
categories <- dedublicated.clean.df$t_label

step1 <- Corpus(VectorSource(texts_short))
step6  <- cleanCorpus(step1)

## without this line predict won't work
dtm_n <- DocumentTermMatrix(step6, control=dict_control)

## creating data.frame for new data to be scored
df_n <- NULL
df_n <- as.data.frame(inspect(dtm_n))

pred_s <- predict(model_s, df_n, decision.values = TRUE)
pred_n <- predict(model_n, df_n, decision.values = TRUE, probability = TRUE)
pred_t <- predict(model_t, df_n, decision.values = TRUE)
 
###################################
df_n$score_svm <- pred_s
df_n$score_naive <- pred_n
df_n$score_tree <- pred_t
df_n$t_source <- "tweeter"
df_n$t_message <- dedublicated.clean.df$t_message

df_n$t_id <- dedublicated.clean.df$t_id
df_n$t_urls <- dedublicated.clean.df$t_urls
df_n$t_users <- dedublicated.clean.df$t_users
df_n$t_time <- dedublicated.clean.df$t_time

df_n$t_sme <- 0
df_n$t_ml <- 0.0
df_n$t_status <- paste0("Scored: ", tobe_scored)

#df_n$t_status.1 <- NULL

########################
# polarity >> library(qdap)
rt2.text <- as.data.frame(df_n$t_message)
pol = 
  lapply(rt2.text, function(txt) {
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
df_n$score_emotion = sapply(pol, function(x) x$all$polarity)
#
names(df_n)

myvars<- c("t_status", "t_id", "t_message" ,"t_time"  ,"t_urls" ,"t_users","t_source" ,"medic", "score_svm", "score_tree","score_naive",
           "score_emotion", "t_ml" ,"t_sme")

tw_short <- NULL
tw_short <- subset(df_n, select=myvars)

#elastic
names(tw_short)
nrow(tw_short)
names(tw_short) <- gsub("\\.", "_", names(tw_short))

name_index <- paste0("tw-pro-", scored_sequence)
docs_bulk(tw_short, index = name_index, type = "ms01", doc_ids = tw_short$t_id)

name_index
count(index=name_index) 

#save.image("C:/03-MyProjects/WebHose/proc0121.RData")

#index_delete(index="tw-pro-01-*")

# not working count(callopts=verbose())



