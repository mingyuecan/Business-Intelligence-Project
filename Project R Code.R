library(xlsx)
library(stringr)
library(plyr)
library(dplyr)

## County Employment 
setwd("/Users/Constance/Desktop/DW_Project")
Employment <- read.xlsx("Employment2017.xls", sheetIndex=1, startRow = 1)

County_Employment<-Employment %>% filter(str_detect(Area.Name, "County"))
County_Employment<-County_Employment[,-1]
County_Employment$County<-County_Employment$County %>% str_replace("County", "")

## 2016 Crime by County 
Crime2016 <- read.xlsx("Crime2016.xlsx", sheetIndex=1, startRow = 1)

Crime2016$County<-as.character(Crime2016$County)
Crime2016$County<-trimws(Crime2016$County, which="right")

Crime2016_County<-ddply(Crime2016, "County", numcolwise(sum))

## County Population and Housing Unit
Population <- read.xlsx("CountyPopulation2016-2017.xlsx", sheetIndex=1, startRow = 1)
HousingUnit <- read.xlsx("HousingUnit_County.xlsx", sheetIndex=1, startRow = 1)

## Bay Area rental price by county
County_OneBedroom<-read.xlsx("County_1Bedroom.xlsx", sheetIndex=1, startRow = 1)
County_TwoBedroom<-read.xlsx("County_2Bedroom.xlsx", sheetIndex=1, startRow = 1)
County_ThreeBedroom<-read.xlsx("County_3Bedroom.xlsx", sheetIndex=1, startRow = 1)

County_OneTwo<-full_join(County_OneBedroom, County_TwoBedroom)
County_RentalPrice<-full_join(County_OneTwo, County_ThreeBedroom)
colnames(County_RentalPrice)<-c("County", "OneBedroom", "TwoBedroom", "ThreeBedroom")
County_RentalPrice$AvgRentalPrice <-
  rowMeans(subset(
    County_RentalPrice,
    select = c("OneBedroom", "TwoBedroom", "ThreeBedroom")
  ), na.rm = TRUE)
County_RentalPrice<-subset(County_RentalPrice, select=c("County", "AvgRentalPrice"))

## Merge Crime, Employement data, poplulation, housing unit, and rental price
Population$County<-as.character(Population$County)
HousingUnit$County<-as.character(HousingUnit$County)

Population$County<-trimws(Population$County, which="right")
HousingUnit$County<-trimws(HousingUnit$County, which="right")
County_Employment$County<-trimws(County_Employment$County, which="right")


PopEmp<-inner_join(Population, County_Employment, by="County")
PopEmpHou<-inner_join(PopEmp, HousingUnit, by="County")
PopEmpHouCrime<-inner_join(PopEmpHou, Crime2016_County, by="County")
CountyAll<-left_join(PopEmpHouCrime, County_RentalPrice, by="County")

write.xlsx(CountyAll, "CountyAll.xlsx", showNA=FALSE, row.names = FALSE)


## City Employment
City_Employment<-Employment %>% filter(str_detect(Area.Name, "city|town"))
City_Employment$County<-City_Employment$County %>% str_replace("County", "")
colnames(City_Employment)[1] <- "City"
City_Employment$City<-City_Employment$City %>% str_replace("city|town", "")

City_Employment$County<-trimws(City_Employment$County, which="right")

BayCounty<-c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")
BayCity_Employment<-filter(City_Employment, County %in% BayCounty)

## City Population
City_Population <- read.xlsx("Population_City.xlsx", sheetIndex=1, startRow = 1)
colnames(City_Population)[2]<-"Population"
City_Population$City<-as.character(City_Population$City)
City_Population$City<-trimws(City_Population$City, which="right")

## City Crime
BayCity_Employment$City<-trimws(BayCity_Employment$City, which="right")
BayCity_Crime<-filter(Crime2016, County %in% BayCounty)
colnames(BayCity_Crime)[2]<-"City"

BayCity_Crime$City<-as.character(BayCity_Crime$City)
BayCity_Crime$City<-trimws(BayCity_Crime$City, which="right")
BayCity_Crime$City<-BayCity_Crime$City %>% str_replace("Suisun", "Suisun City")

## Bay Area
BayArea <- read.xlsx("BayArea.xlsx", sheetIndex=1, startRow = 1)
BayArea$City<-as.character(BayArea$City)
BayArea$City<-trimws(BayArea$City, which="right")

BayCityCrime<-filter(BayCity_Crime, City %in% BayArea$City)

BayCityEmployment<-filter(BayCity_Employment, City %in% BayArea$City)

BayCity_Population<-filter(City_Population, City %in% BayArea$City)

## Combine Rental Price Data for different floor plan
City_OneBedroom<-read.csv("City_1Bedroom.csv")
City_TwoBedroom<-read.csv("City_2Bedroom.csv")
City_ThreeBedroom<-read.csv("City_3Bedroom.csv")

City_OneTwo<-full_join(City_OneBedroom, City_TwoBedroom)
City_RentalPrice<-full_join(City_OneTwo, City_ThreeBedroom)

City_RentalPrice<-City_RentalPrice[City_RentalPrice$City!="Castro Valley",]

## Merge Population, Employment, Crime, Rental Price for Cities in Bay area
BayArea_Pop<-left_join(BayArea, BayCity_Population, by="City")
BayCity_PopEmp<-left_join(BayArea_Pop, BayCityEmployment, by=c("City", "County"))
BayCity_PopEmpCrime<-left_join(BayCity_PopEmp, BayCityCrime, by=c("City", "County"))
BayCity_All<-left_join(BayCity_PopEmpCrime, City_RentalPrice, by=c("City", "County"))

write.xlsx(BayCity_All, "BayCity_All.xlsx", showNA=FALSE, row.names = FALSE)
