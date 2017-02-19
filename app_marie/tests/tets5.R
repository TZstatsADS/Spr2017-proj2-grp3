library(rgdal)

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countries <- readOGR("./data/countries.geojson", "OGRGeoJSON")
map <- leaflet(countries)

binpal <- colorBin("Blues", countries$gdp_md_est, 6, pretty = FALSE)

map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~binpal(countries$gdp_md_est)
  )

pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est
)S
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(countries$gdp_md_est)
  )