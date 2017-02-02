
library(jsonlite)
lapply(c('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
       library, character.only = TRUE)

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

wh <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&q=virtual%20reality%20%20augmented%203d%20medical%20(augmented%20OR%20virtual)%20-(porn%20sex%20game%20video%203g%204g%20mobile%20toys%20gift%20offer%20deal)%20language%3A(english)&ts=1480161288313", flatten = TRUE))
wh_last  <- as.data.frame(fromJSON("https://webhose.io/search?token=49135022-cfa8-4a85-9493-1bb2d4e402a9&format=json&ts=1481927273514&q=virtual+reality++augmented+3d+medical+%28augmented+OR+virtual%29+-%28porn+sex+game+video+3g+4g+mobile+toys+gift+offer+deal%29+language%3A%28english%29", flatten = TRUE))
wh <- rbind( wh, wh_last)

# wh[1,"next."]
# wh[1,"moreResultsAvailable"]
# wh[1,"totalResults"]
#
# baseurl <- "https://projects.propublica.org/nonprofits/api/v1/search.json?order=revenue&sort_order=desc"
# pages <- list()
# for(i in 0:2){
#  mydata <- fromJSON(paste0(baseurl, "&page=", i), flatten=TRUE)
#  message("Retrieving page ", i)
#  pages[[i+1]] <- mydata$filings
#  }
#combine all into one
#   filings <- rbind.pages(pages)
#check output
#   nrow(filings)

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

timeDist = ggplot(wh, aes(t_time)) + 
  geom_density(aes(fill = t_type), alpha = .5) +
  scale_fill_discrete(name="News type") +
  xlab('All Newss') + theme(legend.justification = c(1, 1), legend.position = c(1, 1))

timeDist

#some bots got in on the action too. URL

par(mar = c(3, 3, 3, 2))

wh$t_url_short = strtrim(wh$t_url, 30)

platform.d20 <- as.matrix(sort(table(wh$t_url_short)))
platform.d5 <- platform.d20[which(platform.d20 >1) , ]

dotchart(platform.d5)
mtext('Number of News posted by platform')



# As reality check, what are the most and least positive articles
#wh$t_message[which.min(wh$t_emotional)]

# How does emotionalValence change over the day?

ggplot(wh, aes(t_time, t_emotional)) +
  geom_point() + 
  geom_smooth(span = .5)

#using the sentiment dictionary of Hu & Liu, 2004) identifies as positively or negatively valenced.
polWordTables = 
  sapply(pol, function(p) {
    words = c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '), 
              negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
    gsub('-', '', words)  # Get rid of nothing found's "-"
  }) %>%
  apply(1, paste, collapse = ' ') %>% 
  stripWhitespace() %>% 
  strsplit(' ') %>%
  sapply(table)

par(mfrow = c(1,2))
invisible(
  lapply(1:2, function(i) {
    dotchart(sort(polWordTables[[i]]), cex = .95)
    mtext(names(polWordTables)[i])
  }))


# emotional valence == cloud
polSplit = split(wh, sign(wh$t_emotional))

polText = sapply(polSplit, function(df) {
  paste(tolower(df$text), collapse = ' ') %>%
    gsub(' (http|@)[^[:blank:]]+', '', .) %>%
    gsub('[[:punct:]]', '', .)
}) %>%
  structure(names = c('negative', 'neutral', 'positive'))

# remove emotive words
polText['negative'] = removeWords(polText['negative'], names(polWordTables$negativeWords))
polText['positive'] = removeWords(polText['positive'], names(polWordTables$positiveWords))

# Make a corpus by valence and a wordcloud from it

corp = Corpus(VectorSource(polText))
col3 = RColorBrewer::brewer.pal(3, 'Paired') # Define some pretty colors, mostly for later
wordcloud(as.matrix(TermDocumentMatrix(corp)), 
                            max.words = 100, min.freq = 2, random.order=FALSE, 
                            rot.per = 0, color=pal)



sentence.v <- VectorSource(wh$t_message)
corpus.not.clean <- Corpus(sentence.v)
#
corpus <- tm_map(corpus.not.clean, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#
corpus <- tm_map(corpus, removeNumbers)
# 
# myStopwords<- c(stopwords('english'), "href","what", "often","maybe","will","want","trying")
# corpus<-tm_map(corpus,removeWords,myStopwords)
#corpus<- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)

dtm <- DocumentTermMatrix(corpus)
dtm3 <- removeSparseTerms(dtm, 0.95)
dtm4.m <- as.matrix(dtm3)
View(dtm4.m)


frequency <- colSums(dtm4.m)
mean_95 = sort(colMeans(dtm4.m), decreasing=T)
average_95 <- mean(mean_95[1:30])

barplot(mean_95[1:30], border=NA,las= 3,xlab="Top 30",ylab="Top 30", ylim= c(0,1.0))

mean_95[1:30]
frequency[1:30]
words <- names(frequency)
wordcloud(words, color=pal)
#
findFreqTerms(dtm, lowreq=30)
findFreqTerms(dtm, lowfreq=4)
findAssocs(dtm, 'porn', .1)

wordcloud(corpus,  min.freq=10, color=pal)
#
# ggplot
#
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>75), aes(fill = word, word, freq))
p <- p + geom_bar(stat="identity") + coord_flip()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

tdm <- DocumentTermMatrix(corpus)

#elastic
connect(es_base = "23.101.132.194", es_port = 9220)
connection()
#cleanned

#wh_index <- paste0("wh-c-",Sys.Date())
#count(index=wh_index, type='wh-c') 
#index_delete(wh_index)

#adding back
#rt$receiver <- el$receiver
#
#droping non need columns
colnames(a2)
drop <- c(   "t_url_short", "t_persona" , "t_Organizations", "requestsLeft", "posts.uuid"                   ,           "posts.url"              , "posts.ord_in_thread"           ,          "posts.author"                        ,"posts.published"                    ,"posts.title"                          , "posts.text"                       ,       "posts.highlightText"                     , "posts.highlightTitle"                ,    "posts.language"                       , "posts.external_links"              ,      "posts.crawled"                         , "posts.thread.uuid"                 ,      "posts.thread.url"         ,   "posts.thread.site_full"      ,            "posts.thread.site"                  , "posts.thread.site_section"           ,    "posts.thread.site_categories"           , "posts.thread.section_title"         ,     "posts.thread.title"                    , "posts.thread.title_full"                , "posts.thread.published"              , "posts.thread.replies_count"         ,     "posts.thread.participants_count"        , "posts.thread.site_type"             ,     "posts.thread.country"                   , "posts.thread.spam_score"                , "posts.thread.main_image"              , "posts.thread.performance_score"       ,   "posts.thread.domain_rank"              , "posts.thread.social.facebook.likes"    ,  "posts.thread.social.facebook.comments"  , "posts.thread.social.facebook.shares"   ,  "posts.thread.social.gplus.shares"       , "posts.thread.social.pinterest.shares"  ,  "posts.thread.social.linkedin.shares"    , "posts.thread.social.stumbledupon.shares", "posts.thread.social.vk.shares"          , "posts.entities.persons"        ,          "posts.entities.organizations"           ,"posts.entities.locations"   , "totalResults" , 
          "moreResultsAvailable"             ,       "next.")
wh = wh[,!(names(wh) %in% drop)]

prop.table(table(wh$t_type))
prop.table(table(wh$t_users))

wh_index <- paste0("wh-c-b",Sys.Date())
count(index=wh_index, type='wh-c') 

docs_bulk(wh, index = wh_index, doc_ids = wh$t_id)
count(index=wh_index) 



