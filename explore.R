#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
all_bids_df <- read.csv("auction_bids.csv") %>% 
   group_by(id) %>% mutate(final_amount=max(amount))

p.all<-
   all_bids_df %>% filter(from_last<50) %>%
   ggplot() + aes(x=-from_last, y=bid_ptotal) +
    geom_line(aes(group=id, color=final_amount),alpha=.2)+
    geom_smooth() +
    ggtitle("all auctions: ratio of final price over time") +
    theme_set(cowplot::theme_cowplot()) +
    scale_color_viridis_b() #continuous(low="gray", high="red")
print(p.all)
ggsave(p.all, file="imgs/all_bids_by_auction.png", width=7, height=3)

## subset to play
#bid_data <- all_bids_df %>% filter(id == first(id))
#ggplot(bid_data) +
#    aes(x=since_first, y=amount, color=user_id) +
#    geom_smooth(method='lm', formula=y~x, aes(color=NULL)) +
#    geom_point() +
#    geom_line(aes(group=user_id)) +
#    ggtitle("bids over time") + theme_set(theme_bw())
