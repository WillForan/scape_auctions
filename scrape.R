#!/usr/bin/env Rscript
library(jsonlite)
library(dplyr)
library(lubridate)
library(glue)

url2df <- function(url){
   Sys.sleep(.2) # dont spam the server
   cat("# fetching ", url, "\n")
   curl::curl_fetch_memory(url)$content %>% rawToChar %>% fromJSON()
}
escape_list <- function(lst) {
    escape_comma <- "%2C"
    paste(collapse=escape_comma, lst)
}
fetch_auctions<-function(idxs=1:100, cols=c("type","id","listing","location","items_count","name","status","scheduled_end_time","starts_at")){
    idxlist <- escape_list(idxs)
    collist <- escape_list(cols)
    auctions <- url2df(glue('https://bid.joerpyleauctions.com/api/feed?active=false&include_recently_complete_auctions_to_active=false&indices={idxlist}&fields={collist}'))$results
}
auction_bid_id<-function(a_id)
 url2df(glue("https://bid.joerpyleauctions.com/api/auctions/{a_id}?page=active"))$items_statuses[1,1]
fetch_bids<-function(id="7365947"){
    bids <- url2df(glue("https://bid.joerpyleauctions.com/api/items/{id}/bids?bid_type=public&newer_than="))
}
sec2days <- function(s) as.numeric(s)/(60*60*24)
bid_inc <- function(bids) (bids-lag(bids))/lag(bids)

# get dataframe from json
# and mangle columns
bids_adjust<-function(d)
    bid_data <- d %>%
        select(account_id, item_id, user_id,  status_label, amount, placed_at) %>%
        arrange(item_id, amount, placed_at) %>% # amount is always increasing .. right
        mutate(placed_at=ymd_hms(placed_at),
               since_first=sec2days(placed_at-min(placed_at)),
               from_last=sec2days(max(placed_at)-placed_at),
               bid_ptotal = amount/max(amount),
               bid_inc = bid_inc(amount)) %>%
        mutate_at(c("account_id", "item_id", "user_id"),as.factor)

# get bids for an auction
# also need to get auction id ->bid lookup id
# requiers 2 api queries
bids_from_a_id <- function(a_id)
   tryCatch(auction_bid_id(a_id) %>%
            fetch_bids %>%
            bids_adjust %>%
            mutate(a_id=a_id),
         error=function(e) { print(e); return(NULL) })

# awkward reps b/c rownames block rbind. make a list to combine with dplyr::bind_rows
# NB. fetch_auctions (x1) and bids_from_a_id (x2) introduce sleep/delay via url2df
auctions <- split(1:501, rep(1:5,each=100)) %>% lapply(fetch_auctions) %>% bind_rows
all_bids <- lapply(auctions$id, bids_from_a_id) 
all_bids_df <-bind_rows(all_bids)
all_bids_df <- merge(auctions,all_bids_df,by.x="id",by.y="a_id")
write.csv(all_bids_df, file="auction_bids.csv", row.names=F)
