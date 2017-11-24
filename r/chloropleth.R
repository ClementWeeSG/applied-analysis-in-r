library(leaflet)
library(readr)

wine_file <- read_csv("data/wine_master.utf8.csv")
mapPolys <- geojsonio::geojson_read("data/countries.geo.json", what = "sp")
names(mapPolys) = c("id","country")

column = "value1"

  wine_selection = wine_file[, c("recoded_country","value11")]
  names(wine_selection) = c("country","value")
  
  mapBySelection = aggregate(value ~ country, wine_selection, median, na.action = na.omit)
  merged = sp::merge(mapPolys, mapBySelection)
  color.ramp = colorRamp(c("red","blue"), interpolate = "spline")
  pal <- colorNumeric(color.ramp, domain = merged$value, na.color = "white")
  
  map = leaflet(merged) %>% addPolygons(
    fillColor = ~pal(value),
    weight = 2,
    opacity = 1,
    color = "black",
    fillOpacity = 1.0
  ) %>% addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                  position = "bottomright")
  
map