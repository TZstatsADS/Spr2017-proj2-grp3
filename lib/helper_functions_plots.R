
 
hist_advertising_media <- function(selected_media, advertising) {
  df <- advertising[advertising$media==selected_media,]
  g <- ggplot(df, aes(x=df$year, y=df$spendings/1000)) +
     geom_bar(stat="identity") +
     xlab("Year") +
     ylab("Spendings (in thousands of $)") +
      labs(title="Commercial Spendings on Tobacco")
  p = ggplotly(g)
  return(p)
}

hist_advertising_total <- function(advertising){
  p <- plot_ly(advertising, x=~year, y=~spendings, color=~media, colors="Accent") %>%
    add_bars() %>%
    layout(barmode = "stack",
           title="Commercial Spendings on Tobacco per year",
           xaxis=list(title="Year"),
           yaxis=list(title="Spendings (in $)"))

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

map_leaflet <- function(cate, mortality, year, gender, prevalence,
                        df, mor, pre, sta, states ){
  if (cate=="Mortality") {  
    ds <- mor
    point <- ds[ds$disease==mortality & ds$year==year & ds$class==gender,c("statesAbbr","ratio")]
    point$statesAbbr <- as.character(point$statesAbbr)
    for (i in 1:52) {
      if(sum(df$name[i]==point$statesAbbr)==1) {
        df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
      } else {
        df[i,"ratio"] <-0
      }
    }
    df$radius <- rank(df$ratio)*20/52+5
    df[df$ratio==0,]$radius <- 0
    legend_t<-paste0(round(quantile(df$ratio)[-1]*100,2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
    legend_size <- quantile(df$radius)[-1]
    color <- rep("black",4)
  } else { 
    if(year==2010 | year==2011) {
      df$radius <- 0
      legend_t <- NULL
      legend_size <- NULL
      color <- NULL
    } else { 
      if(year==2012 & (gender=="Female" | gender=="Male")) {
        df$radius <- 0
        legend_t <- NULL
        legend_size <- NULL
        color <- NULL
      } else {
        ds<-pre
        point <- ds[ds$disease==prevalence & ds$year==year & ds$class==gender,c("statesAbbr","ratio")]
        point$statesAbbr <- as.character(point$statesAbbr)
        for (i in 1:52) {
          if(sum(df$name[i]==point$statesAbbr)==1) {
            df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
          } else {
            df[i,"ratio"] <- 0
          }
        }
        df$radius <- rank(df$ratio)*20/52+5
        if (sum(df$ratio==0)!=0) {
          df[df$ratio==0,]$radius <- 0
        }
        legend_t <- paste0(round(quantile(df$ratio)[-1],2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
        legend_size <- quantile(df$radius)[-1]
        color <- rep("black",4)
      }
    }
  }
  
  # Shade of map
  sta1 <- sta[sta$YEAR==year & sta$LocationDesc!="Guam"& sta$Gender==gender & sta$LocationDesc!="Virgin Islands",]
  var <- sta1$Data_Value
  max <- ceiling(max(var)/10)*10
  min <- floor(min(var)/10)*10
  var <- pmax(var, min)
  var <- pmin(var, max)
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  #shades <- colorRampPalette(c("#fee6ce", "#ff5300"))(100)
  shades <- colorRampPalette(c("#c0c0f8", "#09094f"))(100)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  s <- as.character(states$STUSPS)
  order <- rep(NA,length(fills))
  for (i in 1:length(s)){
    order[i] <- which(as.character(sta1$LocationAbbr)==s[i])
  }
  fills1 <- fills[order]
 
  clb <- data.frame("name"=sta1$LocationAbbr[order],"consumption"=sta1$Data_Value[order])
  clb$name <- as.character(clb$name)
  df$name <- as.character(df$name)   
  if ((year == 2010 | year == 2011) & cate == "Disease")  { 
    clb$ratio = "NA"
  } else {for (i in 1:nrow(clb)) {
      if (sum(clb$name[i]==df$name)==1) {
        clb$ratio[i] <- df[which(df$name==clb$name[i]),"ratio"]
      } else {clb$ratio[i]<-NULL}
     }  
    clb$ratio<- round(clb$ratio,2)
         }
  
  dv<-paste( "Consumption:",as.character(sta1$Data_Value[order]),"%")
  dv1<-paste("Ratio:",clb$ratio,"%")
  labels <- sprintf(
    "<strong>%s</strong><br/>%s <br/>%s",
    sta1$LocationDesc[order],dv, dv1
  ) %>% lapply(htmltools::HTML)
  
  leaflet(states) %>% 
    addTiles() %>% 
    addPolygons(
      stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,
      color = fills1,
      label= labels, labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))%>%
    addCircleMarkers(lat=df$lat, lng=df$long, color="red", 
                     radius=df$radius,stroke=F,fillOpacity = 0.8) %>%
    addLegend(
      position = 'topright',
      colors = shades[c(1, 25, 50, 75, 100)],
      labels = legend.text, opacity = 1,
      title = 'Smokers Proportion'
    ) %>%
    setView(lng=-97,lat=40,zoom=4)
}
