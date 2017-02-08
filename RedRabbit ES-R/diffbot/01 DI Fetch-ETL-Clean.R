
library(jsonlite)
library(dplyr)   # distinct
library(stringr)  # str_count
library(elastic)
library(tm)     # text mining package for R
library(e1071)  # package with various machine-learning libraries
library(qdap)   #polarity

dict_control <- readRDS('C:/03-MyProjects/Models/dict_control.rds')

## Getting the trained models
model_s <- readRDS('C:/03-MyProjects/Models/model_s.rds')
model_n <- readRDS('C:/03-MyProjects/Models/model_n.rds')
model_t <- readRDS('C:/03-MyProjects/Models/model_t.rds')

connect(es_base = "23.101.132.194", es_port = 9220)
connection()

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

num=1000
mydata2 <- (fromJSON("https://api.diffbot.com/v3/search?token=9ade437e27ca6fbb44e57d07415ce806&num=1000&col=GLOBAL-INDEX&query=text%3Azspace%20OR%20text%3Aechopixel%20sortby%3Atimestamp", flatten = TRUE))
mydata2.df <- as.data.frame(mydata2$objects)
save(mydata2.df, file="di-source-echopixel-zspace-02-17")

Start=1000
mydata10001 <- (fromJSON("https://api.diffbot.com/v3/search?token=9ade437e27ca6fbb44e57d07415ce806&num=1000&Start=1000&col=GLOBAL-INDEX&query=text%3ATouchSurgery%20OR%20text%3Adaqri%20sortby%3Atimestamp", flatten = TRUE))
mydata10001.df <- as.data.frame(mydata10001$objects)
mydata2.df <- mydata10001.df

save(mydata2.df, file="di-source-daqri- touch-start-1000-02-17")

#check output
nrow(mydata2.df)
colnames(mydata2.df)

#ETL
# to_be_scored    Diffbot 
#######################
t_message <- mydata2.df$text
t_time <- mydata2.df$timestamp
t_urls <- mydata2.df$gburl
t_users <- mydata2.df$author
t_source <- mydata2.df$siteName

sub.df <-  subset(mydata2.df, select = c(docId))
colnames(sub.df) <- c("t_id")

sub.df$t_message <- cleanTweets(t_message)
sub.df$t_time <- t_time
sub.df$t_urls <- t_urls
sub.df$t_users <- cleanTweets(t_users)
sub.df$t_label <- "score"
sub.df$t_source <- cleanTweets(t_source)

#
# DE duplication of rows
nrow(sub.df)

deduplicated.clean.df <- distinct(sub.df, strtrim(t_message, 200), .keep_all = TRUE)
nrow(deduplicated.clean.df)

# remove word size  msg / remove NULL or BLANK MSG
deduplicated.clean.df <- subset(deduplicated.clean.df, nchar(t_message) > 5) 
nrow(deduplicated.clean.df)
# top 500 character for scoring extraction
#nrows <- nrow(deduplicated.clean.df)
  
  texts_short <- strtrim(deduplicated.clean.df$t_message,500) 
  categories <- deduplicated.clean.df$t_label
  
  step1 <- Corpus(VectorSource(texts_short))
  step6  <- cleanCorpus(step1)
  
  ## without this line predict won't work
  dtm_n <- DocumentTermMatrix(step6, control=dict_control)
  df_n <- as.data.frame(inspect(dtm_n))
  
  ########   Scoring
  pred_s <- predict(model_s, df_n, decision.values = TRUE)
  pred_n <- predict(model_n, df_n, decision.values = TRUE, probability = TRUE)
  pred_t <- predict(model_t, df_n, decision.values = TRUE)
  
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
  df_n$t_source <- deduplicated.clean.df$t_source
  
  df_n$t_sme <- 0
  df_n$t_ml <- 0.0
  df_n$t_label <- "score"
  
  df_n$t_status<-  "diffbot daqri touch"
  
  ########################
  # polarity
  #
  rt2.text <- as.data.frame(deduplicated.clean.df$t_message)
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
  
  ########################
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
  
  sub2.df$t_status   <- df_n$t_status
  sub2.df$t_source <- paste0("Diffbot " , sub2.df$t_source)
  
  #####
  nrow(sub2.df)
  colnames(sub2.df)
  
  #elastic
 # count(index='wh-s-0117', type='wh-s') 
  #index_delete("tw-clean17e-dup")
  #
  names(sub2.df) <- gsub("\\.", "_", names(sub2.df))
  docs_bulk(sub2.df, index = "di-pro-b-02-17", type = "ms-01", doc_ids = sub2.df$t_id)
  #
  count('di-pro-b-02-17')
  #index_delete("ms-s02")
  aliases_get("di-pro*") 
  
  
  
  
