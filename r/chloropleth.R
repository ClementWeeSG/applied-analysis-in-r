library(leaflet)
library(readr)

wine_file <- read_csv("data/wine_master.utf8.csv")
mapPolys <- geojsonio::geojson_read("data/countries.geo.json", what = "sp")
names(mapPolys) = c("id","country")

column = "topic1"

  wine_selection = wine_file[, c("recoded_country","price")]
  names(wine_selection) = c("country","price")
  
  mapBySelection = aggregate(wine_selection, list( wine_selection$country), median, na.action = na.omit)
  merged = sp::merge(mapPolys, mapBySelection)
  color.ramp = colorRamp(c("red","blue"), interpolate = "spline")
  pal <- colorBin("Blues", domain = merged$price, bins=10, na.color = "white")
  
  leaflet(merged) %>% addPolygons(
    fillColor = ~pal(price),
    weight = 2,
    opacity = 1,
    color = "black",
    fillOpacity = 1.0
  ) %>% addLegend(pal = pal, values = ~price, opacity = 0.7, title = NULL,
                  position = "bottomright")
  
