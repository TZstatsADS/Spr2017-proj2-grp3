
advertising <- read.csv("../output/advertising.csv", stringsAsFactors = FALSE, sep=",")


#### Consumption Accros States
consumption_us <- read.csv("../data/consumption_tobacco_us.csv")
consumption_us <- read.csv("consumption_tobacco_us.csv", sep=";")
g <- 
  plot_ly(data=consumption_us, x=~year, y=~percentage,
          type="scatter", mode="lines+markers") %>%
  layout(
    title="Smokers in the US",
    yaxis=list(title="Proportion of Smokers (%)", range=c(0,50)),
    xaxis=list(title="Year")
    )
g

#### Comsumption Per State
df1 <- read.csv("../data/Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv")
df2 <- read.csv("../data/Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")
df1 <- read.csv("Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv")
df2 <- read.csv("Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")
df2$DisplayOrder <- NULL
df2$SubMeasureID <- NULL

consumption_states <- rbind(df1,df2)

consumption_states$TopicType <- NULL
consumption_states$DataSource <- NULL
consumption_states$StratificationID1 <- NULL
consumption_states$StratificationID2 <- NULL
consumption_states$StratificationID3 <- NULL
consumption_states$StratificationID4 <- NULL
consumption_states$TopicTypeId <- NULL
consumption_states$TopicId <- NULL
consumption_states$MeasureId <- NULL
consumption_states$GeoLocation <- NULL
consumption_states$Sample_Size <- NULL
consumption_states$High_Confidence_Limit <- NULL
consumption_states$Low_Confidence_Limit <- NULL
consumption_states$Data_Value_Std_Err <- NULL

consumption_status <- 
consumption_states %>%
  subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Smoking Status") %>%
  subset(Gender=="Overall" & Age=="All Ages" & Race=="All Races")

consumption_status <- 
  consumption_status %>%
  subset(LocationAbbr=="AL" & YEAR=="2010")

bar <- 
  ggplot(consumption_status, aes(Response, Data_Value, fill=Response)) +
  geom_bar(stat="identity") +
  xlab("Status") +
  ylab("Number of People (%)") +
  theme(legend.position="none")
ggplotly(bar)

####

consumption_status <- 
  consumption_states %>%
  subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Smoking Status") %>%
  subset(Gender=="Overall" & Age=="All Ages" & Race=="All Races")

consumption_status <- 
  consumption_status %>%
  subset(YEAR=="2010")

p <- ggplot(consumption_status, aes(x=Response, y=Data_Value, fill=LocationDesc, color=Response)) +
  geom_jitter() +
  xlab("Status") +
  ylab("Number of People (%)") +
  theme(legend.position="none")
ggplotly(p)
# Add year 1995 to 2015

####
consumption_status <- 
  consumption_states %>%
  subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Current Smoking") %>%
  subset(Gender=="Overall" & Race=="All Races") %>%
  subset(Age=="18 to 24 Years" | Age=="25 to 44 Years" | Age=="45 to 64 Years" | Age=="65 Years and Older")

consumption_status <- 
  consumption_status %>%
  subset(YEAR=="2010")

p <- ggplot(consumption_status, aes(x=Age, y=Data_Value, fill=LocationDesc, color=Age)) +
  geom_jitter(width=0.25) +
  xlab("Age") +
  ylab("Number of People (%)") +
  theme(legend.position="none")
ggplotly(p)
# Add year 1995 to 2015

#####
p <- plot_ly(consumption_status, y=~Data_Value, color=~Response, 
             type="box", boxpoints = "all", jitter = 0.3) %>%
  layout(title="Proportion of Smokers",
         yaxis=list(title="Proportion of Smokers (%)"))
p

bp <- 
  ggplot(consumption_status, aes(x=Response, y=Data_Value, fill=Response)) + 
  geom_boxplot() +
  geom_jitter(size=1, 
              alpha=0.6,
              pch=1) +
  xlab("Status") +
  ylab("Number of People (%)") +
  theme(legend.position="none")
ggplotly(bp)

pie <- 
  plot_ly(data= consumption_status, labels=~Response, values=~Data_Value,
          type="pie",
          textposition="inside",
          textinfo ="label+percent",
          hoverinfo ="text",
          text=~paste(Data_Value, '%'),
          showlegend=FALSE) %>%
  layout(title = 'Smoking Status',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie

bar <-
  plot_ly(data= consumption_status, x=~Response, y=~Data_Value, frame=~YEAR,
    type="scatter", mode="markers")
bar

### Moving Chart

consumption_status <- 
  consumption_states %>%
  subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Smoking Status") %>%
  subset(Gender=="Overall" & Age=="All Ages" & Race=="All Races")

consumption_status <- 
  consumption_status %>%
  subset(LocationAbbr=="AL")

g <- 
  ggplot(consumption_status, aes(Response, Data_Value, fill=Response)) +
  geom_point(aes(frame="YEAR"))
ggplotly(g)


#### Consumption 
consumption_animated <-
  consumption_states %>%
  subset(TopicDesc=="Cigarette Use (Adults)" & MeasureDesc=="Current Smoking") %>%
  subset(Gender=="Overall" & Age=="All Ages" & Race=="All Races")

data(gapminder, package = "gapminder")
gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()
ggplotly(gg)



df$Education <- NULL

df <- subset(df, Race=="All Races")
df$Race <- NULL

df1 <- subset(df, TopicDesc=="Cigarette Use (Adults)")

smoking_frequency <- subset(df1, MeasureDesc=="Smoking Frequency")
smoking_status <- subset(df1, MeasureDesc=="Smoking Status")
curently_smoking <- subset(df1, MeasureDesc=="Current Smoking")
