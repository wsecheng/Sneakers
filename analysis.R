library(tidyverse)
library(stringr)
library(plotly)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(scales)

#Read in data and rename
sneakers <- read.csv("shoeDataV2.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(sneakers) <- c('Sneaker', 'Sales', 'Premium', 'MarketAvg', 'ProductID', 'Retail', 'ReleaseDate', 'Colorway', 'MarketRange')

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
sneakers <- sneakers[grepl('(?<=Jordan\\s)\\d', sneakers$Sneaker, perl = TRUE),] #Only keep Air Jordan (number) models (exclude Spizike's and Fusions)
sneakers$Colorway <- gsub('Jordan\\s\\d+\\s', '', sneakers$Sneaker)
sneakers$RetroOG <- ifelse(grepl('\\d+\\sOG', sneakers$Sneaker), 'OG', 'Retro')
sneakers$ModelNumber <- as.numeric(str_extract(sneakers$Sneaker, '(?<!\\()\\d+(?!\\))'))
sneakers <- sneakers %>% filter(ModelNumber < 2010)

# Data Anamolies and Missing Sneakers Hard Fixes #
sneakers <- mutate(sneakers, RetroOG = ifelse(ModelNumber == 23 & Year == 2008, 'OG', RetroOG))
bred2001 <- c('Jordan 1 Retro Bred (2001)', 54, 605, 705, '136066-061', 100, '2001-08-25', 'Black/Varsity Red-White', 407, 693, 2001, 'Retro', 1)
royal2001 <- c('Jordan 1 Retro Royal Blue (2001)', 34, 658, 758, '136066-041', 100, '2001-10-06', 'Black/Royal Blue', 547, 1007, 2001, 'Retro', 1)
sneakers <- rbind(sneakers, bred2001, royal2001)
sneakers[numerics] <- sapply(sneakers[numerics], as.numeric)


### Plots ###
g <- ggplot(data = sneakers, aes(x = ModelNumber, y = MarketAvg))
g + geom_point(aes(size = Sales, color = rank(Premium)), alpha = 0.5) +
  ggtitle("Air Jordan Market Averages") +
  xlab("Model Number") +
  ylab("Market Average") +
  scale_x_discrete(breaks=seq(0, 23, 1)) +
  scale_colour_gradient(low="firebrick1", high="forestgreen") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        plot.title = element_text(size = 24, face = "bold"),
        axis.line = element_line(colour = "darkgrey"), 
        legend.position = "none", 
        text=element_text(family="Gill Sans Light")) +
  scale_size(range = c(0, 15)) +
  ylim(0, 1000)



g2 <- ggplot(data = sneakers, aes(x = Retail, y = MarketAvg, size = Sales))
g2 + geom_point(alpha = .3) + geom_abline(slope = 1, intercept = 0) + facet_wrap(~ ModelNumber, ncol = 5) + ylim(0, 500) 

sneakersg3 <- table(filter(sneakers, RetroOG == 'Retro') %>% select(ModelNumber, Year)) %>% melt()

g3 <- ggplot() 
g3 + geom_tile(data = sneakersg3, aes(x = factor(Year), y = factor(ModelNumber), fill = value), width=.9, height=.9) +
     labs(fill='Sneaker Release Volume') +
     ggtitle("Air Jordan Retro Release Heat Map") +
     scale_fill_gradientn(colours=c("lightcyan1","lightgoldenrod1","darksalmon"), labels=c("0", "2", "4", "6", "8", "10+"), breaks = c(0, 2, 4, 6, 8, 10), limits=c(0, 10), oob=squish) +
     
     theme_minimal() +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
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
