#################################
#  SETUP                        #
#################################

library(ggplot2)
library(dplyr)
library(plyr)
library(stringi)
library(wesanderson)

setwd("D:/ISSS616/Project") # please change your directory here
wine <-read.csv("mastersetof_fullscopedata_deduped.csv") # please change your file here
names(wine)[names(wine) == 'recoded_country'] <- 'country'

#################################
#  STRING REPLACE               #
#################################




#################################
#  STAT TESTS                   #
#################################

res <- cor.test(wine$price,wine$points)
res

# test for price by country
resPrice.aov <- aov(price ~ country, data=wine)
summary(resPrice.aov)
tukeyWinePrice <- TukeyHSD(resPrice.aov)
tukeyWinePrice

# test for price by variety
resPriceVar.aov <- aov(price ~ variety, data=wine)
summary(resPriceVar.aov)
tukeyWinePriceVar <- TukeyHSD(resPriceVar.aov)
tukeyWinePriceVar

# test for points by country
resPoints.aov <- aov(points ~ country, data=wine)
summary(resPoints.aov)
tukeyWinePoints <- TukeyHSD(resPoints.aov)
tukeyWinePoints

# test for points by variety
resPointsVar.aov <- aov(points ~ variety, data=wine)
summary(resPointsVar.aov)
tukeyWinePointsVar <- TukeyHSD(resPointsVar.aov)
tukeyWinePointsVar

# corr matrix for topics. 
topics <- c("topic1", "topic2", "topic3","topic4", "topic5", "topic6",
            "topic7", "topic8", "topic9","topic10", "topic11", "topic12",
            "topic13", "topic14", "topic15","topic16", "topic17", "topic18",
            "topic19", "topic20")
wineTopics <- wine[topics]
corrTopics <- cor(wineTopics)
round(corrTopics,2)
install.packages("corrplot")
library(corrplot)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(wineTopics)
corrplot(corrTopics,type = "upper", method="number",  
         order="hclust", p.mat = p.mat, sig.level =0.20,
         insig = "blank")



#################################
#  GGPLOT2 GRAPHS               #
#################################

# Reviews by top 5 countries 
ggplot(subset(wine, 
              country %in% c("France","Italy","Spain","Chile","United States of America")
),
aes(points,fill=country))+  
  geom_bar(na.rm=TRUE,
           position="dodge")+
  theme(legend.position="bottom")+
  scale_x_discrete(name="Review Points") +
  scale_y_continuous(name="Number of Reviews")+
  ggtitle("Reviews by Top 5 Countries")


# histogram by points - overall distribution 
ggplot(wine,aes(points,fill=points))+geom_bar(na.rm=TRUE,fill="blue")+
  ggtitle("Distribution of Reviews by Points")+
  scale_x_continuous(name="Points in Review") +
  scale_y_continuous(name="Number of Reviews")+
  theme(legend.position = "none")

# boxplot by price and points
library(RColorBrewer)
gbox <- ggplot(wine,aes(points,price,group=points,color=points))+
  geom_boxplot(na.rm=TRUE)+
  scale_x_continuous(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of Wine Price by Review Points")+
  theme(legend.position="none")
gbox

# boxplot: by top 12 varieties
ggplot(subset(wine, variety %in% c("Pinot Noir",
                                   "Chardonnay",
                                   "Cabernet Sauvignon",
                                   "Sauvignon Blanc",
                                   "Syrah",
                                   "Riesling",
                                   "Merlot",
                                   "Zinfandel",
                                   "Sangiovese",
                                   "Malbec",
                                   "Red Blend",
                                   "White Blend")),
       aes(points,price,group=points,fill=variety))+
  geom_boxplot(na.rm=TRUE) + 
  scale_x_continuous(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of price by score by top 12 varieties")+
  facet_wrap(~ variety, scales="free")+
  theme(legend.position = "none")






#################################
#  DEPRECATED                   #
#################################


library(RColorBrewer)
ggplot(wine,aes(points,fill=points))+geom_bar(na.rm=TRUE,fill="blue")+
  ggtitle("Distribution of Reviews by Points")+
  scale_x_continuous(name="Points in Review") +
  scale_y_continuous(name="Number of Reviews")+
  theme(legend.position = "none")

ggplot(subset(wine, country %in% c("United States of America","France","Italy","Spain","Chile")))+
  aes(x=points,fill=points,group=points)
+geom_histogram(na.rm=TRUE)



names(wine)[names(wine) == 'recoded_country'] <- 'country'

# boxplot: standard boxplot.
library(RColorBrewer)
gbox <- ggplot(wine,aes(points,price,group=points,color=points))+
  geom_boxplot(na.rm=TRUE)+
  scale_x_continuous(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of Wine Price by Review Points")+
  theme(legend.position="none")
gbox


ggplot(wine,aes(points,price,group=points))+
  geom_boxplot(na.rm=TRUE,fill="blue")





# reviews sorted by country -- quite ugly
ggplot(wine, aes(x=reorder(recoded_country, -table(recoded_country)[recoded_country]))) + 
  geom_bar(na.rm=TRUE)+
  scale_x_discrete(name="Number of Reviews") +
  scale_y_continuous(name="Country")+
  ggtitle("Number of Reviews by Country")


top5countries <- countryCount[rev(order(countryCount$freq)),"recoded_country"][1:5]
top5countries 

countryCount$Group <- ifelse(countryCount$freq %in% top5countries,countryCount$freq,"Other")
countryCount$Group <-factor(countryCount$Group,levels=c(top5countries,"Other"))
countryCount.summary <- ddply(countryCount, .(Group), summarise, total=sum(freq))
countryCount.summary$prop <- countryCount.summary$total / sum(countryCount.summary$total)
countryCount.summary

ggplot(countryCount,aes(x=recoded_country))+geom_bar()


# boxplot: standard boxplot.
gbox <- ggplot(wine,aes(points,price,group=points,fill=country))+geom_boxplot(na.rm=TRUE)
gbox

# wine_top4 <- wine[which(wine3$country == "France"| wine3$country =="Italy"| wine3$country == "Spain"| wine3$country == "Chile")]

ggplot(subset(wine, country %in% c("France","Italy","Spain","Chile","United States of America")),
       aes(points,price,group=points,fill=country))+
  geom_col(na.rm=TRUE)+
scale_x_continuous(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of price by score by top 5 countries")+
  facet_wrap(~ country, scales="free")+
  theme(legend.position = "bottom")



# boxplot: by top 5 countries.
gboxCountry <- ggplot(subset(wine, country %in% c("France","Italy","Spain","Chile","United States of America")),
  aes(points,price,group=points,fill=country))+
  geom_boxplot(na.rm=TRUE) + 
  scale_x_continuous(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of price by score by top 5 countries")+
  facet_wrap(~ country, scales="free")+
  theme(legend.position = "bottom")
gboxCountry



# boxplot: by top 20 varieties
ggplot(subset(wine, variety %in% c("Pinot Noir",
                                   "Chardonnay",
                                   "Cabernet Sauvignon",
                                   "Sauvignon Blanc",
                                   "Syrah",
                                     "Riesling",
                                     "Merlot",
                                     "Zinfandel",
                                     "Sangiovese",
                                     "Malbec",
                                    "Red Blend",
                                   "White Blend")),
  aes(points,price,group=points,fill=variety))+
  geom_boxplot(na.rm=TRUE) + 
  scale_x_continuous(name="Review Points") +
  scale_y_continuous(name="Price of Wine ($)")+
  ggtitle("Boxplot of price by score by top 12 varieties")+
  facet_wrap(~ variety, scales="free")+
  theme(legend.position = "none")

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
ggplot(subset(wine, recoded_country %in% c("France","Italy","United States of America","Chile","Spain")),
       aes(x=points,
           y=price,
           color=recoded_country))+
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



