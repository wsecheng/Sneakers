Air Jordan Resell Market
================

Data Cleansing
--------------

``` r
library(tidyverse)
library(stringr)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(scales)

#Read in data and rename
sneakers <- read.csv("data/shoeDataV2.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(sneakers) <- c('Sneaker', 'Sales', 'Premium', 'MarketAvg', 
                        'ProductID', 'Retail', 'ReleaseDate', 'Colorway', 'MarketRange')

# Regex Cleaning
sneakers <- sneakers[!grepl('GS',sneakers$Sneaker, fixed = TRUE),]
sneakers <- sneakers[grepl('\\$\\d+',sneakers$Retail),]
sneakers$MarketAvg <- gsub('$', '', sneakers$MarketAvg, fixed = TRUE)
sneakers$Premium <- gsub('%', '', sneakers$Premium, fixed = TRUE)
sneakers$Retail <- gsub('$', '', sneakers$Retail, fixed = TRUE)
sneakers$MarketRange <- gsub('$', '', sneakers$MarketRange, fixed = TRUE)
sneakers$MarketRange <- gsub('--', 'NA', sneakers$MarketRange, fixed =TRUE)
sneakers$MarketRange <- gsub(',', '', sneakers$MarketRange, fixed =TRUE)
sneakers$Retail <- gsub(',', '', sneakers$Retail, fixed =TRUE)

# Tidying Up Data
sneakers <- sneakers %>% separate(MarketRange, into = c('MarketLow', 'MarketHigh'), sep = ' - ')
numerics <- c('Sales', 'Premium', 'MarketAvg', 'Retail', 'MarketLow', 'MarketHigh')
sneakers[numerics] <- sapply(sneakers[numerics], as.numeric)

# Date Variables
sneakers$ReleaseDate <- ymd(sneakers$ReleaseDate)
sneakers$Year <- year(sneakers$ReleaseDate)

# Parse Sneaker Names #
sneakers <- sneakers[!grepl('X{2,3}\\d*', sneakers$Sneaker),] #Remove XX4 models and up 
sneakers <- sneakers[grepl('(?<=Jordan\\s)\\d', sneakers$Sneaker, perl = TRUE),] #Only keep Air Jordan 
#(number) models (exclude Spizike's and Fusions)
sneakers$Colorway <- gsub('Jordan\\s\\d+\\s', '', sneakers$Sneaker)
sneakers$RetroOG <- ifelse(grepl('\\d+\\sOG', sneakers$Sneaker), 'OG', 'Retro')
sneakers$ModelNumber <- as.numeric(str_extract(sneakers$Sneaker, '(?<!\\()\\d+(?!\\))'))
sneakers <- sneakers %>% filter(ModelNumber < 2010)

# Data Anamolies and Missing Sneakers Hard Fixes #
sneakers <- mutate(sneakers, RetroOG = ifelse(ModelNumber == 23 & Year == 2008, 'OG', RetroOG))
bred2001 <- c('Jordan 1 Retro Bred (2001)', 54, '', 705, '136066-061', 100, '2001-08-25', 
              'Black/Varsity Red-White', 407, 693, 2001, 'Retro', 1)
royal2001 <- c('Jordan 1 Retro Royal Blue (2001)', 34, '', 758, '136066-041', 100, '2001-10-06', 
               'Black/Royal Blue', 547, 1007, 2001, 'Retro', 1)
sneakers <- rbind(sneakers, bred2001, royal2001)
sneakers[numerics] <- sapply(sneakers[numerics], as.numeric)
```

Data Used in Plots
------------------

This is the final data that will be used to create the data visualizations.

``` r
head(sneakers)
```

    ##                                Sneaker Sales Premium MarketAvg  ProductID
    ## 1               Jordan 3 Retro Crimson   329   -12.5       228 136064-005
    ## 2            Jordan 3 Retro White Flip    43    46.0       253 315767-101
    ## 3         Jordan 7 Retro Premio Bin 23    26   614.3      1243 436206-101
    ## 4 Jordan 18 OG Low Black Silver Chrome     7   -16.7       183 306151-001
    ## 5       Jordan 15 Retro Stealth (2017)   442   -34.2       179 881429-001
    ## 6 Jordan 6 Retro Infrared Black (2014)  1962    83.8       357 384664-023
    ##   Retail ReleaseDate                    Colorway MarketLow MarketHigh Year
    ## 1    160  2013-02-23               Retro Crimson       112        168 2013
    ## 2    150  2007-03-24            Retro White Flip       206        232 2007
    ## 3    175  2010-11-26         Retro Premio Bin 23       947       1553 2010
    ## 4    150  2003-05-31  OG Low Black Silver Chrome        69        181 2003
    ## 5    190  2017-01-07        Retro Stealth (2017)        87        163 2017
    ## 6    185  2014-11-28 Retro Infrared Black (2014)       246        434 2014
    ##   RetroOG ModelNumber
    ## 1   Retro           3
    ## 2   Retro           3
    ## 3   Retro           7
    ## 4      OG          18
    ## 5   Retro          15
    ## 6   Retro           6

To create a heatmap we use the melt() function from the reshape library to create counts by ModelNumber and Year.

``` r
sneakersg3 <- table(filter(sneakers, RetroOG == 'Retro') %>% select(ModelNumber, Year)) %>% melt()
head(sneakersg3)
```

    ##   ModelNumber Year value
    ## 1           1 1999     0
    ## 2          10 1999     0
    ## 3          11 1999     0
    ## 4          12 1999     0
    ## 5          13 1999     0
    ## 6          14 1999     0

Data Visualizations
-------------------

We first examine the overall Air Jordan Resell Market.

``` r
### Plots ###
g <- ggplot(data = sneakers, aes(x = ModelNumber, y = MarketAvg))
g + geom_point(aes(size = Sales, color = rank(Premium)), alpha = 0.5) +
  ggtitle("Air Jordan Market Averages") +
  xlab("Model Number") +
  ylab("Market Average") +
  scale_x_discrete(limits=seq(1, 23, 1)) +
  scale_colour_gradient(low="firebrick1", high="forestgreen") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        plot.title = element_text(size = 24, face = "bold"),
        axis.line = element_line(colour = "darkgrey"), 
        legend.position = "none", 
        text=element_text(family="Gill Sans Light")) +
  scale_size(range = c(0, 15)) +
  ylim(0, 1000)
```

![](writeup_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png) This bubble chart allows us to see the variation in resell prices by shoe model. Quickly we can that some of the most popular Air Jordan models, namely the I, IV, V, XI, and XII have the largest number of secondary sales. Shoe volume quickly tapers off after the XIV, which can be attributed to a couple things.

1.  Jordan stopped playing the Bulls 97-98 season (year of the original XIV release). This is a plausible reason for why Nike has not placed much of an emphasis on retroing these OG models that followed the XIV, therby limiting the amount of secondary sales for these shoes.

2.  Among the sneakerhead community, the XIV is typically used as the cutoff in terms of aesthetically pleasing designs. Not to say XV-XX3 are ugly shoes, but when you consider the classic silhouttes of models such as the I, III-VII, XI, and XII, combined with the fact that Jordan played his prime years in these shoes, its easy to see why a majority of sneakerheads' attention is focused on the earlier models.

The greens and reds in this chart are a measure of the 'premium' that certain shoes hold. Green indicates shoes that have a high premium, that is, its resale price is considerably higher than its retail price. Red indicates shoes with a low resale price, this can mean that the shoe has a resale price that is only slightly higher than its retail price, or even a shoe with a resale price lower than its retail price.

Retail price for Air Jordan Retro's vary by year (it has been going up about $10 every 1-2 years or so), model and exclusivity, but if we use $160 as a rough estimate, we can see some shoes appear to be selling below this mark, particularly many colorways of the 1's. So while the 1's contain some of the most popular and expensive colorways namely shoes like the Bred 1's, Royal 1's, and SBB 1's, there indeed seem to be some retro models of the 1's that do not demand the same hype.

To further examine this phenomena, I created a heatmap of the shoes by year to visualize the release patterns of Air Jordans.

``` r
g3 <- ggplot() 
g3 + geom_tile(data = sneakersg3, aes(x = factor(Year), y = factor(ModelNumber),
                                      fill = value), width=.9, height=.9) +
     labs(fill='Sneaker Release Volume') +
     ggtitle("Air Jordan Retro Release Heat Map") +
     scale_fill_gradientn(colours=c("lightcyan1","lightgoldenrod1","darksalmon"), 
                          labels=c("0", "2", "4", "6", "8", "10+"),
                          breaks = c(0, 2, 4, 6, 8, 10), limits=c(0, 10), oob=squish) +
     theme_minimal() +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), 
           plot.title = element_text(size = 24, face = "bold"), 
           axis.title.x=element_blank(),
           axis.text.x = element_text(angle = -40, hjust = .5),
           axis.title.y=element_blank(),
           axis.text.y=element_text(size=10, hjust=1),
           text=element_text(family="Gill Sans Light"),
           legend.position= 'bottom',
           legend.direction = 'horizontal',
           legend.title.align=0.5,
           legend.key.width=unit(1.25,"cm")) +
      guides(fill = guide_colourbar(title.position = "top")) 
```

![](writeup_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

There are a couple interesting patterns to take from this heatmap. We can see that after 2008 (the year that Nike re-retro'ed every model preceeding the XX3 in the form of "Countdown Packs"), Nike steadily introduced more colorways (original and new) of various models. The 1's in particular have see an massive growth in release volume, beginning in 2012 and has held steady up to date.

Another interesting trend begins in 2009, where it looks like Nike began to commemorate certain models. For example, 2009 was the 23rd anniversary of the OG Air Jordan 1 that released in 1986, 2010 was the 23rd anniversary of the Jordan 2, etc. (notice the staircase pattern beginning in 2009?).

This massive explosion in sneaker release volume has some important implications. From a sneakerhead prospective, many ask themselves if Jordan's are still as 'cool' as they were back in the mid 2000s. There are certainly more Retro Air Jordan's available on the secondary market in 2017, than there were in 2005. And for Nike, does this signal an attempt to grap a share of the resell profit? For many people that used to strike out on Jordan releases in the past and pay resell prices, they now have better chance of securing a pair at retail because Nike has simply released more shoes. It's simply much more difficult for the average sneakerhead to buy up $170-$180 shoes every 2-3 weeks and resell them. So for Nike is this increase in volume its way of maximzing profits in the short term? Or given that MJ is now 54 years old, could this be a signal that the Jordan line be on its way out? Time will tell.
