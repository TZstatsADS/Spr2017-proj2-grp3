states <- readOGR("../data/cb_2015_us_state_20m.shp",
                  layer = "cb_2015_us_state_20m", verbose = FALSE)

neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(states) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~colorQuantile("YlOrRd", states$AWATER)(AWATER)
  )