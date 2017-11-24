# Page 1 renderings

page_1_price_dist <- function(df){
  pricingDensity <- density(na.omit(df$price), bw="sj")
  plot(pricingDensity, main = "Distribution of Wine Prices")
  polygon(pricingDensity, col = "blue", border="red")
}
