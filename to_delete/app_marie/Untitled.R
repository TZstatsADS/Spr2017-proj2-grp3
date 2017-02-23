

library(RColorBrewer)

p <- plot_ly(advertising, x=~year, y=~spendings, color=~media, colors="Accent") %>%
  add_bars() %>%
  layout(barmode = "stack", 
         title="Title",
         xaxis=list(title="Year"), 
         yaxis=list(title="Spendings (in thousands)"))
p

colors <- colorRampPalette(brewer.pal(9, "Accent"))
ngroups <- length(unique(advertising$media))

g <- ggplot(advertising, aes(x=advertising$year, y=advertising$spendings/1000, fill=advertising$media)) +
  geom_bar(stat="identity") +
  xlab("Year") + 
  ylab("Spendings on advertising (in thousands)") +
  scale_fill_manual(name = "Media", values = colors(ngroups))

p <- ggplotly(g)
p

p %>% add_trace(consumption, x =~year, y =~percentage, type="scatter", mode = "line")

p %>% add_trace(x =consumption$year, y =consumption$percentage, type="scatter", mode = "line")
consumption <- read.csv("./data/consumption.csv", sep=";")

df <- advertising[advertising$media=="Newspapers",]
g <- ggplot(df, aes(x=df$year, y=df$spendings/1000)) +
  geom_bar(stat="identity") +
  xlab("Year") + 
  ylab("Spendings on advertising (in thousands of $)")
p = ggplotly(g)


p <- plot_ly() %>%
  add_bars(advertising, x=advertising$year, y=advertising$spendings, 
           color=advertising$media, colors="Accent") %>%
  add_trace(x =consumption$year,
                y =consumption$percentage, type="scatter",
                mode = "line", yaxis="y2") %>%
  layout(barmode = "stack", 
         title="Title",
         xaxis=list(title="Year"), 
         yaxis=list(title="Spendings (in thousands)"),
         yaxis2=ay)
p %>% add_trace(x =consumption$year, y =consumption$percentage, type="scatter", mode = "line", yaxis="y2")



ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)



consumption <- read.csv("./data/consumption_tobacco_us.csv", sep=";")

ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "Tobacco Consumption (%)",
  range=c(0,80)
)

p <- plot_ly() %>%
  add_bars(advertising, x=advertising$year, y=advertising$spendings, 
           color=advertising$media, colors="Accent") %>%
  add_trace(x =consumption$year,
            y =consumption$percentage, type="scatter", 
            mode="line", yaxis="y2",
            name="Tobacco Consumption", line=list(color="black")) %>%
  layout(barmode = "stack", 
         title="",
         xaxis=list(title="Year"), 
         yaxis=list(title="Spendings (in $)"),
         yaxis2=ay,
         legend = list(orientation='h'))


g <- ggplot(df, aes(x=df$year, y=df$spendings/1000)) +
  geom_bar(stat="identity") +
  geom_line(consumption, aes(year, percentage)) +
  xlab("Year") +
  ylab("Spendings on advertising (in thousands of $)")
g

