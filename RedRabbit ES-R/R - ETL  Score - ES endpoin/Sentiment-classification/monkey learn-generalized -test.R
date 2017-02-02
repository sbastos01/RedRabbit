
install.packages("tibble ")
library(tibble)
devtools::install_github("tidyverse/tibble")

cat("GITHUB_PAT=18c5f764360c5ffd460f96c2994ac4661ae447c2\n",
    file=file.path(normalizePath("~/"), ".Renviron"),
    append=TRUE)

cat("MONKEYLEARN_KEY=4d70fac15f62e4effee9053387f4199aa5a0a378\n",
    file=file.path(normalizePath("~/"), ".Renviron"),
    append=TRUE)

Sys.getenv("MONKEYLEARN_KEY") 
Sys.getenv("GITHUB_PAT")
library(RCurl)
library("monkeylearn")
library("elastic")

sessionInfo()

text1 <- "I think this is awesome."
text2 <- "Holy shit! You did great!"
request <- c(text1, text2)

text <- c(df8c$t.message[1:2])
entity_shorts <- monkeylearn_classify(request = text,
                                      classifier_id = "ex_isnnZRbS")

ex_isnnZRbS

ex_y7BPYzNG

text1 <- " Medical imaging startup Echopixel blends  Dprinting and  VR    
607 Levels:         D AIR IMAGE PREVIEW    Electron Holography Analysis Tools opensource  sfjp ..."
text2 <- "i want to buy an iphone"
request <- c(text1, text2)
monkeylearn_classify(request, classifier_id = "cl_oFKL5wft")

output2 <- monkeylearn_extract(text1, extractor_id = "ex_isnnZRbS",
                               params = list(max_keywords = 5))

nrows_entity <- nrow(from_Score_to_seeded)
entity_df <- from_Score_to_seeded

for(i in 1:nrows_entity)
{
  
  text <- c(entity_df$t_message[i])
  # er buss pi_s5izXXhC
  # sentiment   cl_ozN8WAwB
  # entity  ex_isnnZRbS
  print(paste0(i))
  entity_short <- monkeylearn_extract(request = text, extractor_id = "ex_isnnZRbS")
  
  if (nrow(entity_short) >0 )
  {
    entity_short$t_id <- entity_df$t._id[i]
    entity_short$t_message <- entity_df$t.message[i]
    entity_short$t_time <- entity_df$t.time[i]
    entity_short$t_urls <- entity_df$t.urls[i]
    entity_short$text_md5 <- NULL
    entity_short$count<- NULL
    entity_all <- rbind( entity_all, entity_short)
    print(paste0(i, " ", entity_df$t.message[i]))
  }
}
#
entity_df$t.message[482]
#nrow(entity_short)
#nrow(entity_all)

#entity_all <- entity_short


rt2.text <- as.data.frame(entity_all$t_message)
#rt$receiver <- el$receiver

#Add polarity 
pol = 
  lapply(rt2.text, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
entity_all$emotionalValence = sapply(pol, function(x) x$all$polarity)

#elastic
connect(es_base = "23.101.132.194", es_port = 9220)
connection()

count(index='tw-news-blog2', type='tw') 

#rename to match elastic index
#names(d_processed)[names(d_processed)=="text"] <- "t_message"
#names(entity_all)[names(entity_all)=="id"] <- "t_id"
#names(d_processed)[names(d_processed)=="created"] <- "t_time"
#names(d_processed)[names(d_processed)=="screenName"] <- "t_users"
names(entity_all)[names(entity_all)=="emotionalValence"] <- "t_sc_emotion"

entity_all$t_source <- "tweeter"

docs_bulk(entity_all, index = "tw-entity2", type = "entity2", doc_ids = entity_all$t_id)

count(index='tw-entity2', type='entity2') 


