library(dplyr)
dirName <- 'c:\\Users\\pavel\\Downloads\\Software for Data Science\\Lab5'
setwd(dirName)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

print("Question 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
Make a plot showing the total PM2.5 emission from all sources for each of the years
1999, 2002, 2005, and 2008.")
data1 <- NEI %>% group_by(year) %>% summarise(total = sum(Emissions))
barplot(data1$total ~ data1$year, main="Emmissions", xlab="Year", ylab="Tons", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

print("Emissions decreased")

print("Question 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips
== '24510') from 1999 to 2008?")
data2 <- NEI[NEI$fips == "24510",] %>% group_by(year) %>% summarise(total = sum(Emissions))
barplot(data2$total ~ data2$year, main="Emmissions", xlab="Year", ylab="Tons", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

print("Emissions decreased")

print("Question 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
variable, which of these four sources have seen decreases in emissions from 1999–2008 for
Baltimore City? Which have seen increases in emissions from 1999–2008?")
data3 <- NEI[NEI$fips == "24510",] %>% group_by(year, type) %>% summarise(total = sum(Emissions))
barplot(data3$total ~ data3$year + data3$type, main="Emmissions", xlab="Type", ylab="Tons",
col=c("red", "black", "yellow", "blue"), legend = unique(data3$year), beside=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

print("Point increased, everything else decreased")

print("Question 4. Across the United States, how have emissions from coal combustion-related sources
changed from 1999–2008?")
data4 <- NEI[NEI$SCC %in% SCC[grepl("coal", SCC$Short.Name), ]$SCC, ] %>% group_by(year) %>% summarise(total = sum(Emissions))
barplot(data4$total ~ data4$year, main="Coal emmissions", xlab="Year", ylab="Tons", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

print("Coal emissions decreased")

print("Question 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore
City (EI.Sector starts from “Mobile”)?")
data5 <- NEI[NEI$SCC %in% SCC[startsWith(as.character(SCC$EI.Sector), "Mobile"),]$SCC & NEI$fips == "24510", ] %>% group_by(year) %>% summarise(total = sum(Emissions))
barplot(data5$total ~ data5$year, main="Emmissions", xlab="Year", ylab="Tons", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

print("Emissions decreased")

print("Question 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from
motor vehicle sources in Los Angeles County, California (fips == '06037'). Which
city has seen greater changes over time in motor vehicle emissions?")
data6 <- NEI[NEI$SCC %in% SCC[startsWith(as.character(SCC$EI.Sector), "Mobile"),]$SCC & NEI$fips %in% c("24510", "06037"), ] %>% group_by(year, fips) %>% summarise(total = sum(Emissions))
barplot(data6$total ~ data6$year + data6$fips, main="Emmissions", xlab="Fips", ylab="Tons",
col=c("red", "black", "yellow", "blue"), legend = unique(data6$year), beside=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

print("Maryland decreased almost by a half, Los Angeles increased by around 10-15%")