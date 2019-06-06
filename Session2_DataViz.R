## Data Visualization examples
## Last Updated: August 27, 2018

  ## List the libraries
library(forecast)
library(tidyverse)
library(gplots)
library(GGally)
library(scales)
library(mosaic)
library(mapproj)

  ## Read Amtrak data
Amtrak.df <- read.csv("Amtrak data.csv")

  ## Use time series analysis
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))


  ## Read Boston Housing data
housing.df <- read.csv("BostonHousing.csv")
head(housing.df, 9)


  ## Scatter plot 
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "MDEV", ylab = "LSTAT")

  ## alternative plot with ggplot
    ### use alpha for transperancy [0,1]
ggplot(housing.df) + 
  geom_point(aes(x = LSTAT, y = MEDV), color = "navy", alpha = .7) +
  theme_classic()



  ## Barcharts
    ### compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(housing.df$MEDV, 
                           by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV,  names.arg = data.for.plot$CHAS, 
        xlab = "CHAS", ylab = "Avg. MEDV")

  ### alternative plot with ggplot
ggplot(data.for.plot) + 
  geom_bar(aes(x = CHAS, y = MeanMEDV), stat = "identity") +
  theme_classic()

  ### barchart of CHAS vs. % CAT.MEDV
data.for.plot2 <- aggregate(housing.df$CAT..MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot2) <- c("CHAS", "MeanCATMEDV")
barplot(data.for.plot2$MeanCATMEDV * 100,  names.arg = data.for.plot2$CHAS, 
        xlab = "CHAS", ylab = "% of CAT.MEDV")



  ## Histogram 
    ### Histograms of MEDV
hist(housing.df$MEDV, xlab = "MEDV")
  ### alternative plot with ggplot
ggplot(housing.df) +
  geom_histogram(aes(x = MEDV), binwidth = 5) +
  theme_classic()



  ## Boxplots
    ###Boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")
  # alternative plot with ggplot
ggplot(housing.df) +
  geom_boxplot(aes(x = as.factor(CHAS), y = MEDV), 
               fill = "gold1", outlier.color = "firebrick2") + 
  xlab("CHAS") +
  theme_classic()

  ## Side-by-Side boxplots
    ### use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")



  ## Heatmaps

  ### simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv = NA, Colv = NA)

  ### heatmap with values
library(gplots)
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(housing.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))



  ## Multidimensional plots
ggplot(housing.df, aes(y = NOX, x = LSTAT, color= factor(CAT..MEDV))) +
  geom_point(alpha = 0.6) +
  theme_minimal()




  ## Panel plots
    ### Computing mean MEDV by RAD & CHAS
data.for.plot3 <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, housing.df$CHAS), 
                           FUN = mean, drop = FALSE)
names(data.for.plot3) <- c("RAD", "CHAS", "meanMEDV")
ggplot(data.for.plot3) +
  geom_bar(aes(x = as.factor(RAD), y = `meanMEDV`), stat = "identity") +
  xlab("RAD") + facet_grid(CHAS ~ .) + theme_classic()


 
  ## Scatter Plot Matrix
plot(housing.df[, c(1, 3, 12, 13)])
    ### Alternative -- reports correlation coefficient
library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)])

#dev.off()


  ## Rescaling - when it is hard to see patterns using original scales

options(scipen=999) # avoid scientific notation

  ## scatter plot: regular and log scale
plot(housing.df$MEDV ~ housing.df$CRIM, xlab = "CRIM", ylab = "MEDV")
    
    ### to use logarithmic scale set argument log = to either 'x', 'y', or 'xy'. 
plot(housing.df$MEDV ~ housing.df$CRIM, 
     xlab = "CRIM", ylab = "MEDV", log = 'xy')


  ## boxplot: regular and log scale
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, 
        xlab = "CAT.MEDV", ylab = "CRIM")
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, 
        xlab = "CAT.MEDV", ylab = "CRIM", log = 'y')




  ## Scaling up to Large Datasets -- Jitterning, smaller markers, contrasting colors
universal.df <- read.csv("UniversalBank.csv")

library(scales)
plot(jitter(universal.df$CCAvg, 1) ~ jitter(universal.df$Income, 1),
     col = alpha(ifelse(universal.df$Securities.Account == 0, "slategray2", "red"), 0.7),
     pch = 20, log = 'xy', ylim = c(0.1, 10),
     xlab = "Income", ylab = "CCAvg")
  
  ### alternative with ggplot
ggplot(universal.df) +
  geom_jitter(aes(x = Income, y = CCAvg, colour = Securities.Account), show.legend = FALSE) + 
  scale_x_log10(breaks = c(10, 20, 40, 60, 100, 200)) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.4, 0.6, 1.0, 2.0, 4.0, 6.0)) +
  theme_classic()



  ##  Interactive charts
    # check http://spotfire.tibco.com
    # www.tableausoftware.com

  ## Treemaps
    # check: www.drasticdata.nl



  ## Maps
library(mosaic)

gdp.df <- read.csv("gdp.csv", skip = 4, stringsAsFactors = FALSE) 
names(gdp.df)[5] <- "GDP2015"
happiness.df <- read.csv("Veerhoven.csv")

    ### gdp map
mWorldMap(gdp.df, key = "Country.Name", fill = "GDP2015") + 
  coord_map()

    ### well-being map
mWorldMap(happiness.df, key = "Nation", fill = "Score") + coord_map() + 
  scale_fill_continuous(name = "Happiness")
