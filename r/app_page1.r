# Page 1 renderings

library(ggplot2)

page_1_price_dist <- function(df){
  # boxplot: standard boxplot.
  gbox <- ggplot(df,aes(points,price,group=points,fill=country))+geom_boxplot(na.rm=TRUE)
  
  
  # boxplot: standard boxplot.
  gbox <- ggplot(subset(df, country %in% c("France","Italy","Spain","Chile","United States of America")),
                 aes(points,fill=country))+
    geom_bar(na.rm=TRUE,position="dodge")+
    ggtitle("Bar chart of number of reviews by country")
  gbox
}

page_1_variety_dist <- function(df){
  pricingDensity <- density(na.omit(df$price), bw="sj")
  plot(pricingDensity, main = "Distribution of Wine Prices")
  polygon(pricingDensity, col = "blue", border="red")
}

page_1_country_dist <- function(df){
  pricingDensity <- density(na.omit(df$price), bw="sj")
  plot(pricingDensity, main = "Distribution of Wine Prices")
  polygon(pricingDensity, col = "blue", border="red")
}

page_1_ratings_dist <- function(df){
  #TODO ...
}