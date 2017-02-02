
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

wh_last  <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&ts=1481927273514&q=virtual+reality++augmented+3d+medical+%28augmented+OR+virtual%29+-%28porn+sex+game+video+3g+4g+mobile+toys+gift+offer+deal%29+language%3A%28english%29", flatten = TRUE))
wh_aggregated <- rbind( wh07, wh08)



# 
wh02[1,"next."]
# wh[1,"moreResultsAvailable"]
# wh[1,"totalResults"]
#

head(wh)
colnames(wh)

wh$t_id <- wh$posts.uuid
wh$t_users <- cleanTweets(wh$posts.author)
wh$t_title <- cleanTweets(wh$posts.title)

wh$t_persons <- wh$posts.entities.persons
wh$t_locations <- wh$posts.entities.locations
wh$t_organizations <- wh$posts.entities.organizations
  
# Put in local time
wh$t_time = wh$posts.published
wh$t_type <- wh$posts.thread.site_type
wh$t_urls <- wh$posts.url
wh$t_message <- cleanTweets(wh$posts.text)

sentence.v <- VectorSource(wh$t_message)
corpus.not.clean <- Corpus(sentence.v)
#
corpus <- tm_map(corpus.not.clean, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#
corpus <- tm_map(corpus, removeNumbers)
#corpus<- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)

dtm <- DocumentTermMatrix(corpus)
dtm3 <- removeSparseTerms(dtm, 0.95)
dtm4.m <- as.matrix(dtm3)
View(dtm4.m)


tdm <- DocumentTermMatrix(corpus)



#droping non need columns
colnames(a2)
drop <- c(   "t_url_short", "t_persona" , "t_Organizations", "requestsLeft", "posts.uuid"                   ,           "posts.url"              , "posts.ord_in_thread"           ,          "posts.author"                        ,"posts.published"                    ,"posts.title"                          , "posts.text"                       ,       "posts.highlightText"                     , "posts.highlightTitle"                ,    "posts.language"                       , "posts.external_links"              ,      "posts.crawled"                         , "posts.thread.uuid"                 ,      "posts.thread.url"         ,   "posts.thread.site_full"      ,            "posts.thread.site"                  , "posts.thread.site_section"           ,    "posts.thread.site_categories"           , "posts.thread.section_title"         ,     "posts.thread.title"                    , "posts.thread.title_full"                , "posts.thread.published"              , "posts.thread.replies_count"         ,     "posts.thread.participants_count"        , "posts.thread.site_type"             ,     "posts.thread.country"                   , "posts.thread.spam_score"                , "posts.thread.main_image"              , "posts.thread.performance_score"       ,   "posts.thread.domain_rank"              , "posts.thread.social.facebook.likes"    ,  "posts.thread.social.facebook.comments"  , "posts.thread.social.facebook.shares"   ,  "posts.thread.social.gplus.shares"       , "posts.thread.social.pinterest.shares"  ,  "posts.thread.social.linkedin.shares"    , "posts.thread.social.stumbledupon.shares", "posts.thread.social.vk.shares"          , "posts.entities.persons"        ,          "posts.entities.organizations"           ,"posts.entities.locations"   , "totalResults" , 
          "moreResultsAvailable"             ,       "next.")
wh = wh[,!(names(wh) %in% drop)]



wh_index <- paste0("wh-c-b",Sys.Date())
#count(index=wh_index, type='wh-c') 

#docs_bulk(wh, index = wh_index, doc_ids = wh$t_id)
count(index=wh_index) 



