# map all of NSW data
library(rgdal)
library(dplyr)
library(stringr)
library(leaflet) 
library(data.table)
library(htmlwidgets)
library(readxl)

setwd("~/Documents/NSW Covid/NSW")

#Import data - Excel worksheet with daily COVID-19 cases and tests by local government area table from https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-local.aspx
X14July <- read_excel("~/Documents/NSW Covid/NSW LGA COVID tests - clean for R import.xlsx",
                      sheet = "14 July 2021")
X13July <- read_excel("~/Documents/NSW Covid/NSW LGA COVID tests - clean for R import.xlsx",
                      sheet = "13 July 2021")
X12July <- read_excel("~/Documents/NSW Covid/NSW LGA COVID tests - clean for R import.xlsx",
                      sheet = "12 July 2021")
X11July <- read_excel("~/Documents/NSW Covid/NSW LGA COVID tests - clean for R import.xlsx",
                      sheet = "11 July 2021")
X10July <- read_excel("~/Documents/NSW Covid/NSW LGA COVID tests - clean for R import.xlsx",
                      sheet = "10 July 2021")

a2 <- select(X10July,'LGA' = 'Local Government Area','Total' = 'Total tests', 'TR' = 'Test rate (per 1000)') %>% mutate(date = '10-July-2021')
b2 <- select(X11July,'LGA' = 'Local Government Area','Total' = 'Total tests', 'TR' = 'Test rate (per 1000)') %>% mutate(date = '11-July-2021')
c2 <- select(X12July,'LGA' = 'Local Government Area','Total' = 'Total tests','TR' = 'Test rate (per 1000)') %>% mutate(date = '12-July-2021')
d2 <- select(X13July,'LGA' = 'Local Government Area','Total' = 'Total tests','TR' = 'Test rate (per 1000)') %>% mutate(date = '13-July-2021')
e2 <- select(X14July,'LGA' = 'Local Government Area','Total' = 'Total tests','TR' = 'Test rate (per 1000)') %>% mutate(date = '14-July-2021') %>% 
  mutate(change = Total - d2$Total) #calculate change in total number of tests between final and penultimate day 
data <- bind_rows(a2,b2,c2,d2,e2)

#spdf file from https://data.gov.au/data/dataset/nsw-local-government-areas 
my_spdf <- readOGR(dsn='~/Documents/NSW Covid/nsw_lga_polygon_shp/NSW_LGA_POLYGON_shp/NSW_LGA_POLYGON_shp.shp')
my_spdf$NSW_LGA__3 <- str_to_title(my_spdf$NSW_LGA__3) #change LGA names to format consistent with other data set
my_spdf$NSW_LGA__2 <- str_to_title(my_spdf$NSW_LGA__2)

#remove unincorporated LGAs
subset <- my_spdf[my_spdf@data$NSW_LGA__3 != 'Unincorporated',]

#join data
x1 <- rbindlist(lapply(subset@data$NSW_LGA__3, as.data.frame.list), use.names = FALSE) #order of LGAs in the spdf variable
names(x1) <- c("LGA")
x2 <- filter(data, date == '14-July-2021') #taking TR data for one date 
choroData <- left_join(x1,x2) #append test rate data in the same LGA order as the spdf variable

hovertext <- paste(
  "LGA: ", choroData$LGA,"<br/>", 
  "Testing Rate (per 1000 population): ", choroData$TR, "<br/>", 
  "Total tests: ", choroData$Total, "<br/>",
  "Tests in last 24 hour period: ", choroData$change, "<br/>",
  "Last updated: 8pm on ", choroData$date,
  sep="") %>%
  lapply(htmltools::HTML)

#plot LGA boundaries on world map
m <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = subset,
              weight = 2,
              label = hovertext,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              ))