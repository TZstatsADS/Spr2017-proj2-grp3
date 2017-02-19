

hist_advertising_media <- function(selected_media) {
  df <- advertising[advertising$media==selected_media,]
  g <- ggplot(df, aes(x=df$year, y=df$spendings/1000)) +
     geom_bar(stat="identity") +
     xlab("Year") + 
     ylab("Spendings on advertising (in thousands)")
   
   return(g)
 }