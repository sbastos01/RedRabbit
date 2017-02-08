
library(jsonlite)
library(dplyr)   # distinct
library(stringr)  # str_count
library(elastic)

rm(list = ls())              # Remove everything from your current workspace
ls()                         # Anything there? Nope.

sessionInfo()
options("scipen"=100, "digits"=4)
setwd("C:/03-MyProjects/diffbot")
connect(es_base = "23.101.132.194", es_port = 9220)
connection()

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
  tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
  tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
  tweets_cl <- gsub('\\d+', '', tweets_cl)
  tweets_cl <- iconv(tweets_cl, "UTF-8")
  return(tweets_cl)
}

# num = 1000
#mydata2 <- (fromJSON("https://api.diffbot.com/v3/search?token=9ade437e27ca6fbb44e57d07415ce806&num=841&col=GLOBAL-INDEX&query=text%3ATouchSurgery%20OR%20text%3Adaqri%20sortby%3Atimestamp", flatten = TRUE))

mydata2 <- (fromJSON("https://api.diffbot.com/v3/search?token=9ade437e27ca6fbb44e57d07415ce806&num=1000&col=GLOBAL-INDEX&query=text%3ATouchSurgery%20OR%20text%3Adaqri%20sortby%3Atimestamp", flatten = TRUE))

Start=1000
mydata10001 <- (fromJSON("https://api.diffbot.com/v3/search?token=9ade437e27ca6fbb44e57d07415ce806&num=1000&Start=1000&col=GLOBAL-INDEX&query=text%3ATouchSurgery%20OR%20text%3Adaqri%20sortby%3Atimestamp", flatten = TRUE))

mydata2.df <- as.data.frame(mydata10001$objects)
save(mydata2.df, file="di-source-daqri- touch-start-1000-02-17")

#check output
nrow(mydata2.df)
colnames(mydata2.df)

#ETL
# to_be_scored    diffbot
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
deduplicated.clean.df <- subset(deduplicated.clean.df, nchar(t_message) > 5) 
nrow(deduplicated.clean.df)

  
  sub2.df <-  subset(deduplicated.clean.df, select = c(t_id))
  colnames(sub2.df) <- c("t_id")
  
  sub2.df$t_message <- deduplicated.clean.df$t_message
  sub2.df$t_time <- deduplicated.clean.df$t_time
  sub2.df$t_urls <- deduplicated.clean.df$t_urls
  sub2.df$t_users <- deduplicated.clean.df$t_users
  #sub2.df$t_id  <- t_id
  sub2.df$t_source <- deduplicated.clean.df$t_source
  
  sub2.df$medic    <- 0
  sub2.df$score_svm   <- 0
  sub2.df$score_tree    <- 0
  sub2.df$score_naive    <- 0
  sub2.df$score_emotion    <- 0
  sub2.df$t_ml    <- 0
  sub2.df$t_sme    <- 0
  
  sub2.df$t_status   <- "daqri - touchserugy"
  sub2.df$t_source <- paste0("Diffbot " , deduplicated.clean.df$t_source)
  
  +++++++++++++++
  nrow(sub2.df)
  
  #elastic
 # count(index='wh-s-0117', type='wh-s') 
  #index_delete("di-raw-02-17")
  #
  names(sub2.df) <- gsub("\\.", "_", names(sub2.df))
  docs_bulk(sub2.df, index = "di-raw-b-02-17", type = "ms-01", doc_ids = sub2.df$t_id)
  count('di-raw-b-02-17')
  #index_delete("di-raw-b-02-17")
 # 
  aliases_get("di*") 
  
 # write.csv(df, file="out.csv", row.name=FALSE)
  # df = read.csv("out.csv", header=TRUE)
  
  
  
  
