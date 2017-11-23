#################################
#  SETUP                        #
#################################

library(ggplot2)
setwd("C:/Users/sharon.woo/Desktop/ISSS616") # please change your directory here
wine <-read.csv("Wine3.csv") # please change your file here
# recoded: recoded_country to "country", removed column 1. Suggest recoding header names esp points.

#################################
#  GGPLOT2 GRAPHS               #
#################################

# boxplot: standard boxplot.
gbox <- ggplot(wine,aes(points,price,group=points,fill=country))+geom_boxplot(na.rm=TRUE)


# boxplot: standard boxplot.
gbox <- ggplot(subset(wine, country %in% c("France","Italy","Spain","Chile","United States of America")),
  aes(points,fill=country))+
  geom_bar(na.rm=TRUE,position="dodge")+
  ggtitle("Bar chart of number of reviews by country")
gbox

gbox <- ggplot(wine,aes(points,group=points,fill=country))+geom_boxplot(na.rm=TRUE)

# wine_top4 <- wine[which(wine3$country == "France"| wine3$country =="Italy"| wine3$country == "Spain"| wine3$country == "Chile")]

# boxplot: by top 5 countries.
ggplot(subset(wine, country %in% c("France","Italy","Spain","Chile","United States of America")),
  aes(points,price,group=points,fill=country))+
  geom_boxplot(na.rm=TRUE) + 
  scale_x_discrete(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of price by score by top 5 countries")+
  facet_wrap(~ country, scales="free")

# boxplot: by top 20 varieties
ggplot(subset(wine, variety %in% c("Pinot Noir",
                                   "Chardonnay",
                                   "Cabernet Sauvignon",
                                   "Bordeaux-Style Red Blend",
                                   "Red Blend",
                                   "Sauvignon Blanc",
                                   "Syrah",
                                     "Riesling",
                                     "Merlot",
                                     "Zinfandel",
                                     "Sangiovese",
                                     "Malbec",
                                     "White Blend")),
  aes(points,price,group=points,fill=variety))+
  geom_boxplot(na.rm=TRUE) + 
  scale_x_discrete(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of price by score by top 12 varieties")+
  facet_wrap(~ variety, scales="free")

# top 4 countries ex US by points and price.
ggplot(subset(wine, country %in% c("France","Italy","Spain","Chile")),
       aes(y=price,
           x=points,
           color=country))+
  geom_point(na.rm=TRUE,alpha = 0.5,
             size=0.5,
             position=position_jitter(width=0.25,height=0))

  # 2 countries by points and price.
  ggplot(subset(wine, country %in% c("France","Italy")),
         aes(y=price,
             x=points,
             color=country))+
    geom_point(na.rm=TRUE,alpha = 0.5,
               size=0.5,
               position=position_jitter(width=0.25,height=0))


# geom_count by country
ggplot(subset(wine, country %in% c("France","Italy","United States of America","Chile","Spain")),
       aes(x=points,
           y=price,
           color=country))+
  geom_count(na.rm=TRUE)

# ggplot(wine,aes(points,price))+  stat_density2d(aes(fill= ..level..,alpha=..level..),geom="polygon",na.rm=TRUE)
# error with double levels. alpha/fill removed. 

# heatmap
ggplot(wine,aes(points,price))+
  stat_density2d(aes(alpha= ..level..),geom="polygon",na.rm=TRUE)

# comparing heatmaps by country
ggplot(subset(wine, country %in% c("France","Italy")),
       aes(x=points,
           y=price,
           color=country))+
  geom_density2d(na.rm=TRUE)+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)



