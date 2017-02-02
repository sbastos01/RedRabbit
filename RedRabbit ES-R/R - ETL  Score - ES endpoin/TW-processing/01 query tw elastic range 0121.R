library(dplyr)  #bindrows
library(elastic) #
library(sqldf)

connect(es_base = "23.101.132.194", es_port = 9220)
connection()

sessionInfo()
setwd("C:/03-MyProjects")
options("scipen"=100, "digits"=4)

dd1 <- "26"
dd2  <- "26"
day_dd1 <- paste0("2017-01-", dd1)
day_dd2 <- paste0("2017-01-", dd2)

d1<- paste0('{"query":{"bool":{"must":[{"range":{"@timestamp":{"gt":', '"', day_dd1,"T00:00:00",'" ,', '"lt":', '"',day_dd2,"T23:59:59",'"}}}]}},"aggs": { } }')
x1a <- Search("tw04-med3d-func", body = d1, asdf=TRUE, size=10000)
#x1a <- Search("ms-s01", q="zspace", asdf=TRUE, size=10000)

x1a.df <- data.frame(x1a)

d2 <- paste0('{ "query":{ "bool": { "must": [ { "range": { "@timestamp": { "gt":', '"', day_d,"T10:00:00",'" ,', '"lt":', '"', day_d,"T23:59:59",'"  } } } ] } }, "from": 0, "size": 10, "aggs": { } }')
x1b <- Search("tw04-med3d-func", body = d2  ,asdf=TRUE, size=10000)
x1b.df <- as.data.frame(x1b)

#a full inner join (all records from both tables) can be created with the "all" keyword:
 
library(plyr) 

 x1_all2 <- join(x1a.df,x1b.df  , type = "full")

nrow(x1a.df)
nrow(x1b.df)

#colnames(x1a.df)

rownn <- paste0("aa", seq_len(nrow(x1a.df)))
rowbb <- paste0("bb", seq_len(nrow(x1b.df)))
rownames(x1a.df) 
<- NULL
<-   rownn #  x1a.df$hits.hits._id
rownames(x1b.df)<-   rowbb  #x1b.df$hits.hits._id

# Failing
#
x1_all <- bind_rows( df_n03, df_n04)
x1_all <- sqldf("select * df" , row.names = TRUE)

  #, row.names = 1:10000)

save.image("C:/03-MyProjects/0121.RData")
