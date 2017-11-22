library(leaflet)

#Read Data
gdps <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
countries <- geojsonio::geojson_read("countries.geo.json", what = "sp")
names(gdps) = c("name", "GDP", "code")
merged <- sp::merge(countries,gdps)

#Show on Map

pal <- colorBin("Blues", domain = merged$GDP, bins = 5, na.color = "white")

m <- leaflet(merged) 

map = m %>% addPolygons(
  fillColor = ~pal(GDP),
  weight = 2,
  opacity = 1,
  color = "black",
  fillOpacity = 1.0
) %>% addLegend(pal = pal, values = ~GDP, opacity = 0.7, title = NULL,
               position = "bottomright")

map
  
