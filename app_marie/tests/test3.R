
## Test 1
map("state")

## Test 2
library(ggplot2)
library(dplyr)

us <- map_data("state")

arr <- USArrests %>% 
  add_rownames("region") %>% 
  mutate(region=tolower(region))

gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=arr, map=us,
                    aes(fill=Murder, map_id=region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='thistle2', high='darkred', 
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg