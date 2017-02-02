options(nytimes_as_key = "1bb7fdf1585040849052b28536936dfd")
nytapi <- "1bb7fdf1585040849052b28536936dfd"

install.packages("rtimes")
library('rtimes')

library('httr')
res4 <- as_search(q="virtual reality AND medical", begin_date = "20160101", end_date = '20170130', 
                  callopts = verbose(),
                  key= nytapi,
                  page=4
                  )
length (res1$data)
res4$meta$hits
nyt_df = NULL
#res$data[[1]]$keywords[[1]]$value
#res$data[[2]]$document_type

res1$data[[1]]$snippet

nyt_rows <-length (res1$data)
nyt_df = data.frame( t_urls=rep(0, nyt_rows))

for(i in 1:nyt_rows){
  nyt_df$t_urls[i] <- res1$data[[i]]$web_url
  nyt_df$t_message[i] <- res1$data[[i]]$snippet
  nyt_df$t_time[i] <- res1$data[[i]]$pub_date
  nyt_df$t_id[i] <- res1$data[[i]]$`_id`
  #nyt_df$t_organizations[i] <- res$data[[i]]$byline$person[[1]]$organization
  nyt_df$t_users[i] <- res1$data[[i]]$byline$original
  
  nyt_df$t_source[i] <- paste0("NYT- ", res1$data[[i]]$document_type)
}

nyt_all <- rbind(nyt_all, nyt_df)
