# setwd("../Desktop/Coursera R/sour-racer/course 3/week 4")
if(!file.exists("communities.csv")){
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "communities.csv", method = "curl")}
communities <- read.csv("communities.csv")
strsplit(names(communities), "wgtp")[123]

if(!file.exists("gdp.csv")){
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "gdp.csv", method = "curl")}
gdp <- read.csv("gdp.csv", skip = 4)
countries <- gdp[1:190,]
gdpcountriessub <- gsub(",", "", countries[,5])
mean(as.numeric(gdpcountriessub))

countryNames <- countries[,4]
length(grep("^United",countryNames))

if(!file.exists("education.csv")){
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", "education.csv", method = "curl")}
education <- read.csv("education.csv")
logical.edu <- grepl("June", education$Special.Notes) & grepl("Fiscal", education$Special.Notes)
length(which(logical.edu))

amzn = quantmod::getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = zoo::index(amzn)
dates2012 <- sampleTimes[grep("2012", sampleTimes)]
length(dates2012)
days2012 <- weekdays(as.Date(dates2012))
length(grep("Monday", days2012))