library(RTextTools)
library(e1071)
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
library(stringr)
library(LSAfun)

#http://link.springer.com/article/10.3758/s13428-014-0529-0

sessionInfo()
setwd("C:/03-MyProjects")
#library(wordnet)

connect(es_base = "23.101.132.194", es_port = 9220)
connection()

options("scipen"=100, "digits"=4)

library(jsonlite)
lapply(c('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
       library, character.only = TRUE)

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

wh <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&q=virtual%20reality%20%20augmented%203d%20medical%20(augmented%20OR%20virtual)%20-(porn%20sex%20game%20video%203g%204g%20mobile%20toys%20gift%20offer%20deal)%20language%3A(english)&ts=1480161288313", flatten = TRUE))
wh_2  <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&ts=1482835692243&q=virtual+reality++augmented+3d+medical+%28augmented+OR+virtual%29+-%28porn+sex+game+video+3g+4g+mobile+toys+gift+offer+deal%29+language%3A%28english%29", flatten = TRUE))
wh_3  <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&ts=1483705365366&q=virtual+reality++augmented+3d+medical+%28augmented+OR+virtual%29+-%28porn+sex+game+video+3g+4g+mobile+toys+gift+offer+deal%29+language%3A%28english%29", flatten = TRUE))
wh_4  <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&ts=1484616762857&q=virtual+reality++augmented+3d+medical+%28augmented+OR+virtual%29+-%28porn+sex+game+video+3g+4g+mobile+toys+gift+offer+deal%29+language%3A%28english%29", flatten = TRUE))



wh_all <- NULL
wh_all <- rbind( wh, wh_last)

#
wh_3[1,"next."]
#wh[1,"moreResultsAvailable"]

# 1205
tw.df2 <- data.frame(wh_all)

#ETL
# to_be_scored    WEBHOSE 
#######################
t_message <- tw.df2$posts.text
t.time <- tw.df2$posts.published
t.urls <- tw.df2$posts.url
t.users <- tw.df2$posts.author
t_organizations <- tw.df2$t_organizations
t_persons <- tw.df2$t_persons
t_locations <- tw.df2$t_locations
#tw.df2$posts.uuid

sub.df <-  subset(tw.df2, select = c(posts.uuid))
colnames(sub.df) <- c("t_id")

t.sentence <- cleanTweets(t_message)

sub.df$t_organizations  <- t_organizations
sub.df$t_persons   <- t_persons
sub.df$t_locations <- t_locations

sub.df$t_message <- t.sentence
sub.df$t_time <- t.time

# what time was in in New-York
#sub.df$t.time.ny <-   ymd_hms(t.time, tz="America/New_York")

sub.df$t_urls <- t.urls
sub.df$t_users <- t.users
sub.df$t_label <- "score"
#
# DE duplication of rows
depublicated.clean.df <- distinct(sub.df, strtrim(t_message, 200), .keep_all = TRUE)

# remove word size  msg / remove NULL or BLANK MSG
depublicated.clean.df <- subset(depublicated.clean.df, 
                                nchar(t_message) >5 & nwords(t_message) >1) 
#
##########################################
###
# top most important sentences extraction
#
nrows <- nrow(depublicated.clean.df)
depublicated.clean.df$t_summary <- "0"
depublicated.clean.df$t_summary_clean <- "0"
depublicated.clean.df$t_lenght <- 00

for(i in 1:nrows) {
  t.sentence <- depublicated.clean.df$t_message[i]
  print(paste0(i," ", nchar(t.sentence)))
  txt_sum <- try(genericSummary(t.sentence,k=3, min=5, split=c(".","!","?")))
  
  txt_summary <- paste0("1-", txt_sum[1], ". 2-", txt_sum[2], ". 3-",txt_sum[3],".")
  txt_summary <- paste0(txt_summary,tw.df2[i,]$t_persons,", ", tw.df2[i,]$t_organizations,", ", tw.df2[i,]$t_locations)
  txt_summary_clean <- paste0(txt_sum[1],". ", txt_sum[2], ". ",txt_sum[3],".")
  
  depublicated.clean.df[i,]$t_summary <- txt_summary
  depublicated.clean.df[i,]$t_summary_clean <- txt_summary_clean
  depublicated.clean.df[i,]$t_lenght <- nchar(t.sentence)

  print(paste0("Processing >>> ", depublicated.clean.df[i,]$t_summary))
  #print(paste0(txt_summary,tw.df2[i,]$t_persons,", ", tw.df2[i,]$t_organizations,", ", tw.df2[i,]$t_locations))
  #
  # failed to summarize -- will get first 2000 char
  for(i in 1:nrows) {
   # strtrim(depublicated.clean.df[i,]$t_summary, 7) 
  
    if  ( strtrim(depublicated.clean.df[i,]$t_summary_clean, 8) == "Error in") 
      {
       txt_summary2 <- paste0(strtrim(depublicated.clean.df[i,]$t_message,2000),". ", depublicated.clean.df[i,]$t_persons, ". ", depublicated.clean.df[i,]$t_organizations, ". ", depublicated.clean.df[i,]$t_locations)
    depublicated.clean.df[i,]$t_summary <-  ""
    depublicated.clean.df[i,]$t_summary <-  txt_summary2
    print(paste0("Processing >>> ", i, "  ", strtrim(depublicated.clean.df[i,]$t_summary, 20)))
    
    depublicated2.clean.df<- depublicated2.clean.df[-which(is.na(depublicated2.clean.df$t_summary)), ]
    # check depublicated.clean.df<- depublicated2.clean.df
    
    }
  }
  
}


#
texts_short <- depublicated.clean.df$t_summary
categories <- depublicated.clean.df$t_label

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

df_n$t_message <- depublicated.clean.df$t_message
df_n$t_summary <- depublicated.clean.df$t_summary
df_n$Correct <- NULL

df_n$t_id <- depublicated.clean.df$t_id
df_n$t_urls <- depublicated.clean.df$t_urls
df_n$t_users <- depublicated.clean.df$t_users

df_n$t_organizations <- depublicated.clean.df$t_organizations
df_n$t_persons <- depublicated.clean.df$t_persons
df_n$t_locations <- depublicated.clean.df$t_locations

df_n$t_time <- depublicated.clean.df$t_time

df_n$t_sme <- 0
df_n$t_ml <- 0.01
df_n$t_label <- "score"
df_n$t_source <- "WEbHose"


########################
# polarity
#
rt2.text <- as.data.frame(df_n$t_summary)
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

#only_seeded_df <- subset(df_n, t_label !=  score_naive & t_label != "seeded")
#only_seeded_all <- rbind(only_seeded_df, only_seeded_df02)
#View(only_seeded_df)

#save.image("C:/03-MyProjects/score08.RData")

#elastic

count(index='wh-s-0104', type='wh-s') 
#index_delete("tw-clean-de-dup")
#
names(df_n) <- gsub("\\.", "_", names(df_n))
docs_bulk(df_n, index = "wh-s-0104", type = "wh-s", doc_ids = df_n$t_id)
#count('wh-s-0104')
#index_delete("plos")
#aliases_get() 
