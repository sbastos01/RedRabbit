#library(RTextTools)
#library(caret)
library(tm)     # text mining package for R
library(e1071)  # package with various machine-learning libraries
library(party)

library(dplyr)   # distinct
library(qdap)   #polarity

library(elastic)
library(stringr)  # str_count
library(LSAfun)  # genericSummary
library(IEEER)

#
library(devtools)
#
install_github("ropensci/IEEER")

sessionInfo()
options("scipen"=100, "digits"=4)
setwd("C:/03-MyProjects")
connect(es_base = "23.101.132.194", es_port = 9220)
connection()

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

ieee_ar_m <- IEEE_search(query = list(ab="augumented reality AND mediCal", pys=2016, pye=2017 ), limit=1000)
ieee_vr_m <- IEEE_search(query = list(ab="virtual reality AND mediCal", pys=2016, pye=2017 ), limit=500)

#IEEE
wh_all <- NULL
tw.df2 <- rbind( ieee_vr_m, ieee_ar_m)
names(tw.df2)

#ETL
# to_be_scored    IEEE 
#######################
t_message <- tw.df2$abstract
t_time <- tw.df2$py
t_urls <- tw.df2$mdurl
t_users <- tw.df2$authors
t_source <- tw.df2$pubtype

count(tw.df2)

sub.df <-  subset(tw.df2, select = c(arnumber))
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
count(sub.df)
deduplicated.clean.df <- distinct(sub.df, strtrim(t_message, 200), .keep_all = TRUE)
count(deduplicated.clean.df)

# remove word size  msg / remove NULL or BLANK MSG
deduplicated.clean.df <- subset(deduplicated.clean.df, 
                                nchar(t_message) >5 & nwords(t_message) >1) 
#
# top most important sentences extraction
#
nrows <- nrow(deduplicated.clean.df)
deduplicated.clean.df$t_summary <- "0"
#i <- 1
for(i in 1:nrows) {
  t_sentence <- deduplicated.clean.df$t_message[i]
  print(paste0(i," ", nchar(t_sentence)))
  txt_sum <- try(genericSummary(t_sentence,k=3, min=5, split=c(".","!","?")))
  txt_summary_clean <- paste0("1-", txt_sum[1],". 2-", txt_sum[2], ". 3-",txt_sum[3],"." )
  txt_summary_clean <- paste0(txt_summary_clean, " Summary: " , nchar(txt_summary_clean), " Total:" ,nchar(t_sentence) )
  deduplicated.clean.df$t_summary[i] <- txt_summary_clean
  
  print(paste0("Processing >>> ", deduplicated.clean.df$t_summary[i]))
  
}
#i
for(i in 1:nrows) {
  #shortning if summary fail
  if (nchar(deduplicated.clean.df$t_summary[i]) <= 50) {
    deduplicated.clean.df$t_summary[i] <- strtrim(deduplicated.clean.df$t_message[i],500)
    print(paste0("Processing >>> ", i, "  ", strtrim(deduplicated.clean.df$t_summary[i], 20)))
  }
}
#
texts_short <- deduplicated.clean.df$t_summary
categories <- deduplicated.clean.df$t_label

step1 <- Corpus(VectorSource(texts_short))
step6  <- cleanCorpus(step1)

## without this line predict won't work
dict_control <- readRDS('C:/03-MyProjects/Models/dict_control.rds')
model_s <- readRDS('C:/03-MyProjects/Models/model_s.rds')
model_n <- readRDS('C:/03-MyProjects/Models/model_n.rds')
model_t <- readRDS('C:/03-MyProjects/Models/model_t.rds')

dtm_n <- DocumentTermMatrix(step6, control=dict_control)

## creating data.frame for new data to be scored
df_n <- as.data.frame(inspect(dtm_n))

pred_s <- predict(model_s, df_n, decision.values = TRUE)
pred_n <- predict(model_n, df_n, decision.values = TRUE, probability = TRUE)
pred_t <- predict(model_t, df_n, decision.values = TRUE)

###################################

df_n$score_svm <- pred_s
df_n$score_naive <- pred_n
df_n$score_tree <- pred_t

df_n$t_message <- deduplicated.clean.df$t_message
#df_n$t_summary <- NULL
df_n$Correct <- NULL

df_n$t_id <- deduplicated.clean.df$t_id
df_n$t_urls <- deduplicated.clean.df$t_urls
df_n$t_users <- deduplicated.clean.df$t_users

df_n$t_time <- deduplicated.clean.df$t_time

df_n$t_sme <- 0
df_n$t_ml <- 0.01
df_n$t_label <- "score"
df_n$t_source <- deduplicated.clean.df$t_source

########################
# polarity
#
rt2.text <- as.data.frame(deduplicated.clean.df$t_summary)
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


sub2.df <-  subset(df_n, select = c(t_id))
colnames(sub2.df) <- c("t_id")

sub2.df$t_message <- df_n$t_message
sub2.df$t_time <- df_n$t_time
sub2.df$t_urls <- df_n$t_urls
sub2.df$t_users <- df_n$t_users
#sub2.df$t_id  <- t_id
sub2.df$t_source <- df_n$t_source

sub2.df$medic    <- df_n$medic
sub2.df$score_svm   <- df_n$score_svm
sub2.df$score_tree    <- df_n$score_tree
sub2.df$score_naive    <- df_n$score_naive
sub2.df$score_emotion    <- df_n$score_emotion
sub2.df$t_ml    <- df_n$t_ml
sub2.df$t_sme    <- df_n$t_sme

sub2.df$t_status   <- "IEEE 2016 - Jan17"

#Aggregating.df <-sub2.df
sub2.df$t_source <- paste0("IEEE " , sub2.df$t_source)

names(sub2.df)
nrow(sub2.df)


#elastic
count(index='wh-s-0117', type='wh-s') 
#index_delete("tw-clean17e-dup")
#
names(sub2.df) <- gsub("\\.", "_", names(sub2.df))
docs_bulk(sub2.df, index = "ie-pro-2016-0117", type = "ms-01", doc_ids = sub2.df$t_id)
#count('wh-s-0104')
index_delete("ms-s02")
aliases_get() 

save.image("C:/0201.RData")
