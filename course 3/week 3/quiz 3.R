setwd("C:/Users/Mysterial/Desktop/Coursera R/course 3/week 3")
#question 1
myurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
if(!file.exists("./data.csv")){
        download.file(myurl, "data.csv")
}
houseData <- read.csv("data.csv")
library(dplyr)
#answer > agricultureLogical <- agricultureLogical <- houseData$ACR == 3 & houseData$AGS == 6
#which(agricultureLogical)

#question 2
jpegurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
if(!file.exists("./jeff.jpg")){
        download.file(jpegurl, "./jeff.jpg", method = "curl")
}
library(jpeg)
jpegdata <- readJPEG("jeff.jpg", native = TRUE)
#answer > quantile(jpegdata, probs = c(30, 80))

#question 3
gdpurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
eduurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
gdpData <- read.csv(url(gdpurl), skip = 4, nrows = 231)
eduData <- read.csv(url(eduurl))
#Tidying the data before using - changing gdp class factor to numeric, extracting countries, arranging by gdp
gdpData <- gdpData[!gdpData$X == "",]
gdpData <- mutate(gdpData, gdp = as.numeric(gsub(",", "", X.4)))
gdpData.countries <- gdpData[1:190, ]
gdpData.countries.sort <- arrange(gdpData.countries, gdp)
#answer > table(!is.na(match(gdpData.countries$X, eduData$CountryCode)))
#gdpData.countries.sort[13, 4]


#question 4
gdpData.countries <- mutate(gdpData.countries, 
                            Income.Group = eduData$Income.Group[match(gdpData.countries$X, eduData$CountryCode)])
#answer > gdpData.countries.grouped <- group_by(gdpData.countries, Income.Group)
#summarise(gdpData.countries.grouped, mean(X.1))

#question 5
gdp.ranking <- quantile(gdpData.countries$X.1, probs = c(0.2, 0.4, 0.6, 0.8, 1))
#answer > table(gdpData.countries[1:38, "Income.Group"])

#To check if gdpData mutated with Income.Group from eduData is correctly mapped. Does this by matching Income.Group
#in gdpData with Income.Group in Data.
#checker <- function(y){
#        gdp <- select(mutate(gdpData, Income.Group = eduData$Income.Group[match(gdpData$X, eduData$CountryCode)]), c(X, Income.Group))[y,]
#        edu <- select(eduData, c(CountryCode, Income.Group))[eduData$CountryCode == as.character(select(mutate(gdpData, Income.Group = eduData$Income.Group[match(gdpData$X, eduData$CountryCode)]), c(X, Income.Group))[y,][[1]]),]
#        gdp[[2]] == edu[[2]]
#}

#checklist <- as.vector(NULL)
#for(i in 1:228){
#             if(!length(checker(i)) == 0){
#                     checklist[i] <- checker(i)
#             }
#        }