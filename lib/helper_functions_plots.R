
# Histogram Advertising Spendings for Tobacco Per Media 
hist_advertising_media <- function(selected_media, advertising) {
  df <- advertising[advertising$media==selected_media,]
  g <- ggplot(df, aes(x=df$year, y=df$spendings/1000, fill="#FA8258")) +
     geom_bar(stat="identity", color="#F5BCA9") +
     xlab("Year") +
     ylab("Spendings (in thousands of $)") +
      labs(title="Commercial Spendings on Tobacco") +
    theme(legend.position="none")
  p = ggplotly(g)
  return(p)
}

# Histogram Total Advertising Spendings for Tobacco
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

# Consumption accross Ages
consumption_ages <- function(consumption_states, year){
  consumption_status <- 
    consumption_states %>%
    subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Current Smoking") %>%
    subset(Gender=="Overall" & Race=="All Races") %>%
    subset(Age=="18 to 24 Years" | Age=="25 to 44 Years" | Age=="45 to 64 Years" | Age=="65 Years and Older")
  
  consumption_status <- 
    consumption_status %>%
    subset(YEAR==year)
  
  p <- ggplot(consumption_status, aes(x=Age, y=Data_Value, fill=LocationDesc, color=Age)) +
    geom_jitter(width=0.25) +
    xlab("Age") +
    ylab("Number of People (%)") +
    theme(legend.position="none") +
    labs(title="Proportion of Smokers for different Ages for all States")
  return(ggplotly(p))
}

# Consumption accros status (never smoken, former smoker, current smoker)
consumption_type <- function(consumption_states, year){
  consumption_status <- 
    consumption_states %>%
    subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Smoking Status") %>%
    subset(Gender=="Overall" & Age=="All Ages" & Race=="All Races")
  
  consumption_status <- 
    consumption_status %>%
    subset(YEAR==year)
  
  p <- ggplot(consumption_status, aes(x=Response, y=Data_Value, fill=LocationDesc, color=Response)) +
    geom_jitter() +
    xlab("Smoker Status") +
    ylab("Number of People (%)") +
    theme(legend.position="none") +
    labs(title="Current Smoker vs Former Smoker vs Non-Smokers for all States")
  return(ggplotly(p))
}

# Map
map_leaflet <- function(cate, mortality, year, gender, prevalence,
                        df, mor, pre, sta, states ){
  
  # Merge Mortality data set with dataframe with center coordinates based on state name
  if (cate=="Mortality") {  
   ds <- mor
   point <- ds[ds$disease==mortality & ds$year==year & ds$class==gender,c("statesAbbr","ratio")]
   point$statesAbbr <- as.character(point$statesAbbr)
    
    # Assign mortality ratio to data frame. If not, set the rate to 0
    for (i in 1:52) {
      if(sum(df$name[i]==point$statesAbbr)==1) {
        df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
      } else {
        df[i,"ratio"] <-0
      }
    }
    
    # Assign radius of the circle based on the ratio
    df$radius <- rank(df$ratio)*20/52+5
    df[df$ratio==0,]$radius <- 0
    legend_t<-paste0(round(quantile(df$ratio)[-1]*100,2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
    legend_size <- quantile(df$radius)[-1]
    color <- rep("black",4)
    
    # Merge Prevelance data set with data frame with center coordinates based on state name
  } else { 
   
    # Some years and genders has missing value. So assign value as NULL
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
  
  # Assign shade of each states based on tobacco consumption
  sta1 <- sta[sta$YEAR==year & sta$LocationDesc!="Guam"& sta$Gender==gender & sta$LocationDesc!="Virgin Islands",]
  var <- sta1$Data_Value
  max <- 25
  min <- 5
  var <- pmax(var, min)
  var <- pmin(var, max)
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  #shades <- colorRampPalette(c("#fee6ce", "#ff5300"))(100)
  shades <- colorRampPalette(c("#EFEFFB", "#0101DF"))(100)
  percents <- as.integer((var-5)*5)
  fills <- shades[percents]
  s <- as.character(states$STUSPS)
  order <- rep(NA,length(fills))
  for (i in 1:length(s)){
    order[i] <- which(as.character(sta1$LocationAbbr)==s[i])
  }
  fills1 <- fills[order]
 
  # Merge data set of shade of each states with data frame with center coordinates
  clb <- data.frame("name"=sta1$LocationAbbr[order],"consumption"=sta1$Data_Value[order])
  clb$name <- as.character(clb$name)
  df$name <- as.character(df$name)   
  for (i in 1:nrow(clb)) {
      if (sum(clb$name[i]==df$name)==1) {
        clb$ratio[i] <- round(as.numeric(df[which(df$name==clb$name[i]),"ratio"]),2)
      } else {clb$ratio[i]<-0}
     } 
     
  # Assign popup value
  dv<-paste( "Smokers Proportion:",as.character(sta1$Data_Value[order]),"%")
  dv1<-paste("disease rate:",clb$ratio,"%")
  labels <- sprintf(
    "<strong>%s</strong><br/>%s <br/>%s",
    sta1$LocationDesc[order],dv, dv1
  ) %>% lapply(htmltools::HTML)
  
  # Build the map
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
    addLegend('bottomright',
              title = "Disease Ratio",
              colors=rep("red",2),
              labels=list(paste("Max:",round(max(df$ratio),2),"%"),
                          paste("Min:",round(min(df$ratio),2),"%")),opacity=0.8
             ) %>%
    setView(lng=-97,lat=40,zoom=4)
}
