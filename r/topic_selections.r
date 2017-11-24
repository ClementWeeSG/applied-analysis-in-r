library(dplyr)

wine_topic_suggestions = function(betas){
  reactive({
    search = input$searchTerm
    summed_searches = c()
    if(search == '')
      summed_searches = apply(betas, 2, sum)
    else {
      raw_searches = subset(betas, grepl(input$searchTerm, betas$term))
      summed_searches = apply(raw_searches, 2, sum)
    }
    all_topics = sort(summed_searches, descreasing=T)
    names(all_topics)
  })
}

best_topic = function(betas){wine_topic_suggestions(betas)[1]}

make.chloropleth = function(master){
  reactive({
    wines_selection = master[, c("country",input$topicSelection)]
    names(wine_selection) = c("country","presence")
    mapBySelection = aggregate(wines_by_price, list( wines_selection$country), median, na.action = na.omit)
    color.ramp = colorRamp(c("red","blue"), interpolate = "spline")
    pal <- colorNumeric(color.ramp, domain = wines_selection$weight, na.color = "white")
    
    leaflet(sp::merge(mapPolys,wine_selection)) %>% addPolygons(
      fillColor = ~pal(presence),
      weight = 2,
      opacity = 1,
      color = "black",
      fillOpacity = 1.0
    ) %>% addLegend(pal = pal, values = ~presence, opacity = 0.7, title = NULL,
                    position = "bottomright")
  })
}