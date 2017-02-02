# Load
library("elastic")
library("lubridate")
library(stringr)
library(tm)

sessionInfo()
options(stringsAsFactors = FALSE)
options("scipen"=100, "digits"=4)
setwd("C:/03-MyProjects")
connect(es_base = "23.101.132.194", es_port = 9220)
connection()

dd1 <- "28"
dd2  <- "28"
day_dd1 <- paste0("2017-01-", dd1)
day_dd2 <- paste0("2017-01-", dd2)

d1<- paste0('{"query":{"bool":{"must":[{"range":{"@timestamp":{"gt":', '"', day_dd1,"T00:00:00",'" ,', '"lt":', '"',day_dd2,"T10:59:59",'"}}}]}},"aggs": { } }')
tw.half01 <- Search("tw04-med3d-func", body = d1, asdf=TRUE, size=10000)
tw.half01.df <- data.frame(tw.half01)

d2<- paste0('{"query":{"bool":{"must":[{"range":{"@timestamp":{"gt":', '"', day_dd1,"T11:00:00",'" ,', '"lt":', '"',day_dd2,"T23:59:59",'"}}}]}},"aggs": { } }')
tw.half02 <- Search("tw04-med3d-func", body = d2, asdf=TRUE, size=10000)
tw.half02.df <- data.frame(tw.half02)

# error duplicate row names
# tw.df2 <- rbind( tw.half01.df, tw.half02.df)
tw.df2 <- tw.half01.df

######################   ETL 1
#ETL
t_message <- tw.df2$hits.hits._source$message
t_time <- tw.df2$hits.hits._source$'@timestamp'
t_urls <- tw.df2$hits.hits._source$source
t_users <- tw.df2$hits.hits._source$user
t_source <- "tweeter"

medic <-  " "
score_svm <- " "
score_tree <- " "
score_naive <- " "
score_emotion <- " "
t_ml <- " "
t_sme <- " "
t_status <- "ETL 01-27 tw04-med3d-func"

sub.df <-  subset(tw.df2, select = c(hits.hits._id))
colnames(sub.df) <- c("t_id")

#t_sentence <- cleanTweets(t_message)
sub.df$t_message <- t_message
sub.df$t_time <- t_time
sub.df$t_urls <- t_urls
sub.df$t_users <- t_users
sub.df$t_source <- t_source

sub.df$medic    <- medic
sub.df$score_svm   <- score_svm
sub.df$score_tree    <- score_tree
sub.df$score_naive    <- score_naive
sub.df$score_emotion    <- score_emotion
sub.df$t_ml    <- t_ml
sub.df$t_sme    <- t_sme
sub.df$t_status   <- t_status

  ###############  14 variables
names(sub.df)
nrow(sub.df)

#upload to ES
names(sub.df) <- gsub("\\.", "_", names(sub.df))
docs_bulk(sub.df, index = "tw-raw-01-28", type = "ms01", doc_ids = sub.df$hits.hits._id)

aliases_get("tw-raw*")
count(index='tw-raw-01-28-2')
#index_delete("tw-raw-01-28-2")# Curl options library("httr") 
# docs_bulk(sub.df, index = "seed-pro-01", type = "ms01", doc_ids = sub.df$hits.hits._id,  config=verbose()
