library(leaflet)

make.chloropleth <- function(data, domain, fillC, legendValues){
  pal <- colorBin("Blues", domain = domain, bins = 5, na.color = "white")
  
  m <- leaflet(data) 
  
  map = m %>% addPolygons(
    fillColor = fillC,
    weight = 2,
    opacity = 1,
    color = "black",
    fillOpacity = 1.0
  ) %>% addLegend(pal = pal, values = legendValues, opacity = 0.7, title = NULL,
                  position = "bottomright")
  
  map
}


  
