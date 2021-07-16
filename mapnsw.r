# map all of NSW data
library(rgdal)
library(leaflet) 
library(dplyr)
library(stringr)
library(data.table)
library(readr)
library(lubridate)
library(htmlwidgets) -

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

#shp file from https://data.gov.au/data/dataset/nsw-local-government-areas 
fullSpdf <- readOGR(dsn='~/Documents/NSW Covid/nsw_lga_polygon_shp/NSW_LGA_POLYGON_shp/NSW_LGA_POLYGON_shp.shp')
fullSpdf$NSW_LGA__3 <- str_to_title(fullSpdf$NSW_LGA__3) #change LGA names to format consistent with other data set
fullSpdf$NSW_LGA__2 <- str_to_title(fullSpdf$NSW_LGA__2)

#remove unincorporated LGAs
subset <- fullSpdf[fullSpdf@data$NSW_LGA__3 != 'Unincorporated',]

#join data
x1 <- rbindlist(lapply(subset@data$NSW_LGA__3, as.data.frame.list), use.names = FALSE) #order of LGAs in the spdf variable
names(x1) <- c("LGA")
x2 <- filter(data, date == max(data$date)) #taking TR data for most recent date 
choroData <- left_join(x1,x2) #append test rate data in the same LGA order as the spdf variable

#hovertext
hovertext <- paste(
  "LGA: ", choroData$LGA,"<br/>", 
  "Testing Rate (per 1000 population): ", choroData$TR, "<br/>", 
  "Total tests: ", choroData$Total, "<br/>",
  "Tests in last 24 hour period: ", choroData$change, "<br/>",
  "Last updated: 8pm on ", choroData$date,
  sep="") %>%
  lapply(htmltools::HTML)

#colour scale
binpal <- colorBin("BuPu", choroData$TR,9)

#plot LGA boundaries on world map
m <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = subset,
              weight = 2,
              stroke = TRUE, 
              color = "#444444",
              fillOpacity = 1,
              fillColor = ~binpal(choroData$TR),
              label = hovertext,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              )) %>%
  addLegend(pal = binpal, values = choroData$TR, opacity=0.9, title = "Testing Rates (per 1000 population)", position = "bottomright")