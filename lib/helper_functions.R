

hist_advertising_media <- function(selected_media) {
  df <- advertising[advertising$media==selected_media,]
  g <- ggplot(df, aes(x=df$year, y=df$spendings/1000)) +
     geom_bar(stat="identity") +
     xlab("Year") + 
     ylab("Spendings on advertising (in thousands of $)")
  p = ggplotly(g)
  return(p)
}

hist_advertising_total <- function(){
  p <- plot_ly(advertising, x=~year, y=~spendings, color=~media, colors="Accent") %>%
    add_bars() %>%
    layout(barmode = "stack", 
           title="Title",
           xaxis=list(title="Year"), 
           yaxis=list(title="Spendings (in thousands of $)"))
  
  # colors <- colorRampPalette(brewer.pal(9, "Accent"))
  # ngroups <- length(unique(advertising$media))
  # g <- ggplot(advertising, aes(x=advertising$year, y=advertising$spendings/1000, fill=advertising$media)) +
  #   geom_bar(stat="identity", position="stack") +
  #   xlab("Year") + 
  #   ylab("Spendings on advertising (in thousands)") +
  #   scale_fill_manual(name = "Media", values = colors(ngroups))
  # p <- ggplotly(g)
  # Comment: ggplotly doesn't return the good values
  
  return(p)
}
