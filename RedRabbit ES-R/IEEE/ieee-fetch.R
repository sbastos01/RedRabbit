library(devtools)
install_github("ropensci/IEEER")

library(IEEER)
z3 <- IEEE_search(query = list(ab="augumented reality AND clinical", pys=2016, pye=2017 ), limit=300)
nrow(z)
z[,c("authors", "title")]

options(nytimes_as_key = "1bb7fdf1585040849052b28536936dfd")
nytapi <- "1bb7fdf1585040849052b28536936dfd"

install.packages("rtimes")
library('rtimes')

library('httr')
res2 <- as_search(q="virtual reality AND medical", begin_date = "20150101", end_date = '20170101', 
                  callopts = verbose(),
                  key= nytapi,
                  page=2
                  )
length (res2$data)
res2$meta$hits

#res$data[[1]]$keywords[[1]]$value
#res$data[[2]]$document_type

res2$data[[1]]$snippet


nyt_rows <-length (res2$data)
nyt_df = NULL
nyt_df = data.frame( t_urls=rep(0, 10))


for(i in 1:nyt_rows){
  nyt_df$t_urls[i] <- res$data[[i]]$web_url
  nyt_df$t_message[i] <- res$data[[i]]$snippet
  nyt_df$t_time[i] <- res$data[[i]]$pub_date
  nyt_df$t_id[i] <- res$data[[i]]$`_id`
  #nyt_df$t_organizations[i] <- res$data[[i]]$byline$person[[1]]$organization
  nyt_df$t_persons[i] <- res$data[[i]]$byline$original
  
  nyt_df$source <- paste0("NYT-", res$data[[i]]$document_type)
}

