
# wehhose feature extraction to elasticsearch columns
# Seetings extract column as list to a string add metadata.
#39 Persons
#40 Organizations
#41 Locations

#tw.df2 is a temporary frame of a search query.

tw.df2$t_persons <- " "
tw.df2$t_organizations <- " "
tw.df2$t_locations <- " "

per_39 <- 39 # WH column
org_40 <- 40
loc_41 <- 41

  entity_num <- org_40
  entity_label <- "Organizations: "
  outer_row <- nrow(tw.df2)
  for(i in 1:outer_row)
{
  inner_row <- nrow(tw.df2[[i,entity_num]])
  #
  if (inner_row > 0) 
  { 
    names_all <- entity_label
    for(ii in 1:inner_row)
    {
    if (ii==1 ) {
                 names_all <- paste0(names_all, tw.df2[[i,entity_num]]$name[ii])
              }
      else {
      #
             names_all <- paste0(names_all, ", ",tw.df2[[i,entity_num]]$name[ii])
       }
    }
    print(paste(i, inner_row, names_all))
    if ( entity_num == 39) tw.df2[i,]$t_persons <- names_all
    if ( entity_num == 40) tw.df2[i,]$t_organizations <- names_all
    if ( entity_num == 41) tw.df2[i,]$t_locations <- names_all 
  }
}