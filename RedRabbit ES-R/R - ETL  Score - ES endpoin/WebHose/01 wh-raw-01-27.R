
library(jsonlite)
library(elastic)

#elastic
connect(es_base = "23.101.132.194", es_port = 9220)
connection()

lapply(c('dplyr', 'ggplot2', 'lubridate', 'tm'),library, character.only = TRUE)

#function to clean data
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
  return(tweets_cl)
}
# 49135022-cfa8-4a85-9493-1bb2d4e402a9
#wh <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&ts=1481220001624&q=%22boston+scientific%22+%28medical+OR+clinical+OR+echopixel%29+language%3A%28english%29&sort=relevancy&from=0", flatten = TRUE))

#last 10 days 0127
query01 <- "http://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&q=medical%20(virtual%20OR%20augumented%20OR%203d)%20-(sex%20deal%20girl%20girls%20porno%20printing%20printer%20mobile%203g%204g%20vagina%20game)%20language%3A(english)&sort=relevancy&ts=1484687599874"

from00 <- '&from=00'
wh00 <- as.data.frame(fromJSON(paste0(query01,from00), flatten = TRUE))

from100 <- '&from=100'
wh100 <- as.data.frame(fromJSON(paste0(query01,from100), flatten = TRUE))

from200 <- '&from=200'
wh200 <- as.data.frame(fromJSON(paste0(query01,from200), flatten = TRUE))

from300 <- '&from=300'
wh300 <- as.data.frame(fromJSON(paste0(query01,from300), flatten = TRUE))

from400 <- '&from=400'
wh400 <- as.data.frame(fromJSON(paste0(query01,from400), flatten = TRUE))

from500 <- '&from=500'
wh500 <- as.data.frame(fromJSON(paste0(query01,from500), flatten = TRUE))

from600 <- '&from=600'
wh600 <- as.data.frame(fromJSON(paste0(query01,from600), flatten = TRUE))

wh_aggregated <- rbind( wh00, wh100)
wh_aggregated <- rbind( wh_aggregated, wh200, wh300)
wh_aggregated <- rbind( wh_aggregated, wh400, wh500, wh600)

#  echopixel zspace daqri
#relevancy last 10  0127
query02 <- "http://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&q=(echopixel%20OR%20zspace%20OR%20daqri)%20-(sex%20deal%20girl%20girls%20porno%20printing%20printer%20mobile%203g%204g%20vagina%20game)%20language%3A(english)&sort=relevancy&ts=1484685845031"

from00 <- '&from=00'
wh00 <- as.data.frame(fromJSON(paste0(query02,from00), flatten = TRUE))
wh00 [1,"moreResultsAvailable"]

from100 <- '&from=100'
wh100 <- as.data.frame(fromJSON(paste0(query02,from100), flatten = TRUE))

#final aggrate
wh <- rbind( wh_aggregated, wh00)

# wh02[1,"next."]
# wh[1,"moreResultsAvailable"]
# wh[1,"totalResults"]
#
head(wh)
colnames(wh)
t_message <- cleanTweets(wh$posts.text)
t_time = wh$posts.published
t_urls <- wh$posts.url
t_users <- cleanTweets(wh$posts.author)
t_source <- wh$posts.thread.site

medic <-  " "
score_svm <- " "
score_tree <- " "
score_naive <- " "
score_emotion <- " "
t_ml <- " "
t_sme <- " "
t_status <- "ETL wh-01-27"

t_id <- wh$posts.uuid
#=========================================
  
  sub.df <-  subset(wh, select = c(posts.uuid))
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

###############  13 variables + t_source >>> 14 var
names(sub.df)
# [1] "t_id"          "t_message"     "t_time"        "t_urls"        "t_users"       "t_source"     
# [7] "medic"         "score_svm"     "score_tree"    "score_naive"   "score_emotion" "t_ml"         
# [13] "t_sme"         "t_status" 
nrow(sub.df)

#upload to ES
names(sub.df) <- gsub("\\.", "_", names(sub.df))
docs_bulk(sub.df, index = "wh-raw-01-27", type = "ms01", doc_ids = sub.df$hits.hits._id)

aliases_get("wh-raw*")
count(index='wh-raw-01-27')


