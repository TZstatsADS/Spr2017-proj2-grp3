
# Obtain coordinates of different states
states_coordinates <- function(states) {
   coords_list <- list()
   for (i in 1:52) {
     coords_list[[i]]<- slot(states@polygons[[i]]@Polygons[[1]],"coords")
   }
   f <- function(x){apply(x,2,mean)}
   mean_coords_list <- lapply(coords_list,f)
   name <- as.character(states$STUSPS)
   long <- c()
   lat <-c()
   for (i in 1:52) {
     long[i]<- mean_coords_list[[i]][1]
     lat[i]<- mean_coords_list[[i]][2]
   }
   
   df <- data.frame(name,long,lat)
   df[df$name=="AK",c(2,3)] <- c(-150,66) #change AK coordinates
   df[df$name=="WA",c(2,3)] <- c(-120,47.2) #change WA coordinates
   df[df$name=="CA",c(2,3)] <- c(-118.4541,37) #change CA coordinates
   df[df$name=="WI",c(2,3)] <- c(-88.5,44) #change WI coordinates
   df[df$name=="FL",c(2,3)] <- c(-82,28) #change FL coordinates
   df[df$name=="OH",c(2,3)] <- c(-82.5,40.5) #change OH coordinates
   df[df$name=="NY",c(2,3)] <- c(-74.5,42.59) #change OH coordinates
   df[df$name=="LA",c(2,3)] <- c(-91,30) #change LA coordinates
   df[df$name=="MS",c(2,3)] <- c(-89.5,32.5) #change MS coordinates
   df[df$name=="AL",c(2,3)] <- c(-86.5,32.5) #change AL coordinates
   
   return(df)
 }
 
 
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
