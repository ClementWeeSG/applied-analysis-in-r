# Page 1 - First Tab

pricingDensity <- density(na.omit(wine_master$price), bw="sj")
plot(pricingDensity, main = "Distribution of Wine Prices")
polygon(pricingDensity, col = "blue", border="red")