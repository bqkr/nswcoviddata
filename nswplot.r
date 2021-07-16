library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)
library(data.table)
library(readr)
library(lubridate)
setwd("~/Documents/NSW Covid/NSW/data")

#Read all CSVs in folder
temp = list.files(pattern="*.csv")
data <- as_tibble(rbindlist(lapply(temp, fread),idcol=TRUE)) %>% 
  mutate(.id = sub('\\.csv$', '', temp[.id])) #create a date column 
data <- rename(data, date = .id, 'LGA' = 'Local Government Area','Total' = 'Total tests', 'TR' = 'Test rate (per 1000)')

#calculate change in total tests between latest and previous day -- inefficient 
latest <- data %>% filter(date == date(max(data$date)))
penultimate <- data %>% filter(date == date(max(data$date)) - days(1))
change <- c(latest$Total - penultimate$Total)
withChange <- data %>% filter(date == date(max(data$date))) %>% mutate(change = change) 
data <- left_join(data, withChange)
rm(latest,penultimate, withChange) #remove working variables

#filter to Sydney LGAs
sydneyLGAs <- c('Hornsby',
                'Hunters Hill',
                'Ku-Ring-Gai',
                'Lane Cove',
                'Mosman',
                'North Sydney',
                'Northern Beaches',
                'Ryde',
                'Willoughby',
                'Bayside',
                'Georges River',
                'Randwick',
                'Sutherland Shire',
                # 'Sydney', #use Sydney to initialise the data
                'Waverley',
                'Woollahra',
                'Fairfield',
                'Liverpool',
                'Wingecarribee',
                'Wollondilly',
                'Canterbury-Bankstown',
                'Campbelltown',
                'Camden',
                'Burwood',
                'Canada Bay',
                'Inner West',
                'Strathfield',
                'Blacktown',
                'Cumberland',
                'Parramatta',
                'The Hills Shire',
                'Penrith')

#initialise single LGA variable prior to loop
singleLGA <- data %>% group_by(LGA) %>% filter(LGA == 'Sydney') 
sydneyData <- singleLGA

#separate plots for each LGA, change in testing rate
for(locale in sydneyLGAs) {
  singleLGA <- data %>% group_by(LGA) %>% filter(LGA == locale) 
  sydneyData <- rbind(sydneyData,singleLGA)
  plot <- ggplot(singleLGA, aes(date,TR,group=1)) + geom_line() + geom_point() + ylim(0,680) + 
    ggtitle(locale) + ylab('Testing rate (per 1000 population)') 
  ggsave(plot,filename=paste("sydney",locale,".png",sep=""))
}

#single plot- Fairfield removed as outlier
filteredData <- filter(sydneyData, date == max(sydneyData$date)) 

plot <- ggplot(filteredData, aes(TR,change,group=LGA,colour=LGA)) + 
  geom_point(size = 2) + 
  ggtitle(paste('Testing rate vs Tests in last 24 hour period as at 8pm on',  max(filteredData$date))) +
  xlab('Testing rate (per 1000 population)') + ylab('Tests in last 24 hour period') +
  geom_text_repel(aes(label = LGA)) +
  ylim(0, 3000)
  