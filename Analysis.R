packages <- c("sqldf","dplyr","rowr","reshape2","ggplot2","data.table")
lapply(packages, library, character.only = TRUE)
#load packages

dfstation <- read.csv("C:/workspaceR/BART/station_info.csv")
df2016 <- read.csv("C:/workspaceR/BART/date-hour-soo-dest-2016.csv")
df2017 <- read.csv("C:/workspaceR/BART/date-hour-soo-dest-2017.csv")
#load dataframes

str(dfstation)
str(df2016)
str(df2017)

colSums(is.na(dfstation))
colSums(is.na(df2016))
colSums(is.na(df2017))

z <- df2016 %>% 
  group_by(Origin) %>%
  summarise(no_rows = length(Origin))
View(z)
#examine structure, check for NAs, count how many values per level in df2016$Origin

colnames(dfstation)[colnames(dfstation)=="ï..Abbreviation"] <- "Abbreviation"

levels(dfstation$Abbreviation)[match("WARM",levels(dfstation$Abbreviation))] <- "WSPR"
levels(dfstation$Name)[match("Warm Springs/South Fremont (WARM)",levels(dfstation$Name))] <- "Warm Springs/South Fremont (WSPR)"
#clean data

df2016$Date <- as.Date(df2016$DateTime)
df2016$Time <- format(as.POSIXct(df2016$DateTime) ,format="%H:%M:%S")
df2017$Date <- as.Date(df2017$DateTime)
df2017$Time <- format(as.POSIXct(df2017$DateTime) ,format="%H:%M:%S")

df2016$Weekday <- weekdays(df2016$Date)
df2017$Weekday <- weekdays(df2017$Date)
#data wrangling

odlist <- list(df2016$Origin, df2017$Origin, df2016$Destination, df2017$Destination)
do.call(cbind.fill, c(odlist, fill=NA))

list2matrix <- function(odlist){
  sapply(odlist, "length<-",max(lengths(odlist)))
}

matrixod <- list2matrix(odlist)
od <- as.data.frame(matrixod)
colnames(od) <- c("2016o", "2017o","2016d","2017d")
#create df with all 'Origins' and 'Destinations'

od$`2016o` <- as.numeric(od$`2016o`)
od$`2017o` <- as.numeric(od$`2017o`)
od$`2016d` <- as.numeric(od$`2016d`)
od$`2017d` <- as.numeric(od$`2017d`)

meltod <- reshape2::melt(od, variable.name ="Year", value.name = "Stations")

od2 <- summarise(group_by(meltod,Year,Stations),Combined =n())
od2 <- as.data.frame(od2)
od2 <- od2[complete.cases(od2),]
#df with Year, Stations, and Number of 'Origins' or 'Destinations'. Remove NAs.

od2$Stations <- as.factor(od2$Stations)

plotod <- ggplot(od2, aes(x = Stations, y = Combined))+
  geom_col(aes(fill = Year), width = 0.7)
plotod
#plot Year, Stations, Combined(Origins+Destinations)
## 1. Which BART station is the busiest? Answer=Powell St. (POWL - Station 34)

dfroute2016 <- data.frame(df2016$Origin, df2016$Destination, df2016$Throughput)
colnames(dfroute2016) <- c("Origin", "Destination", "Throughput")

dfroute2016$dummy <- ifelse(dfroute2016$Origin == dfroute2016$Destination, '1', '0')
dfroute2016 <- dfroute2016[dfroute2016$dummy == '0',]
dfroute2016 <- dfroute2016[-4]

dfroute2016sum <- aggregate(.~Origin+Destination, dfroute2016, sum)

dfroute2017 <- data.frame(df2017$Origin, df2017$Destination, df2017$Throughput)
colnames(dfroute2017) <- c("Origin", "Destination", "Throughput")

dfroute2017$dummy <- ifelse(dfroute2017$Origin == dfroute2017$Destination, '1', '0')
dfroute2017 <- dfroute2017[dfroute2017$dummy == '0',]
dfroute2017 <- dfroute2017[-4]

dfroute2017sum <- aggregate(.~Origin+Destination, dfroute2017, sum)
## 2. What is the least popular BART route? 
##2016 Answer = 8 routes had only 1 person pass through. 1.WSPR-PHIL 2.WSPR-SHAY 3.19th-WSPR 4.CAST-WSPR 5.EMBR-WSPR 6.FRMT-WSPR 7.LAKE-WSPR 8.SHAY-WSPR
##2017 Answer = WDUB to NCON (34 people passed through)

df31 <- data.frame(df2016$Origin, df2016$Destination, df2016$Throughput, df2016$Time)
colnames(df31) <- c("Origin", "Destination", "Throughput", "Time")
df32 <- data.frame(df2017$Origin, df2017$Destination, df2017$Throughput, df2017$Time)
colnames(df32) <- c("Origin", "Destination", "Throughput", "Time")

df3 <- rbind(df31,df32)

df3$dummy <- ifelse(df3$Origin == df3$Destination, '1', '0')
df3 <- df3[df3$dummy == '0',]
df3 <- df3[-5]

df3 <- df3[!(df3$Origin=="EMBR"),]
df3 <- df3[!(df3$Origin=="MONT"),]
df3 <- df3[!(df3$Origin=="POWL"),]
df3 <- df3[!(df3$Origin=="CIVC"),]
df3 <- df3[!(df3$Origin=="16TH"),]
df3 <- df3[!(df3$Origin=="24TH"),]
df3 <- df3[!(df3$Origin=="GLEN"),]
df3 <- df3[!(df3$Origin=="BALB"),]
df3 <- df3[!(df3$Origin=="DALY"),]
df3 <- df3[!(df3$Origin=="COLM"),]
df3 <- df3[!(df3$Origin=="SSAN"),]
df3 <- df3[!(df3$Origin=="SBRN"),]
df3 <- df3[!(df3$Origin=="MLBR"),]

df3 <- df3[!(df3$Destination=="RICH"),]
df3 <- df3[!(df3$Destination=="DELN"),]
df3 <- df3[!(df3$Destination=="PLZA"),]
df3 <- df3[!(df3$Destination=="NBRK"),]
df3 <- df3[!(df3$Destination=="DBRK"),]
df3 <- df3[!(df3$Destination=="ASHB"),]
df3 <- df3[!(df3$Destination=="MCAR"),]
df3 <- df3[!(df3$Destination=="19TH"),]
df3 <- df3[!(df3$Destination=="12TH"),]
df3 <- df3[!(df3$Destination=="WOAK"),]
df3 <- df3[!(df3$Destination=="PITT"),]
df3 <- df3[!(df3$Destination=="NCON"),]
df3 <- df3[!(df3$Destination=="CONC"),]
df3 <- df3[!(df3$Destination=="PHIL"),]
df3 <- df3[!(df3$Destination=="WCRK"),]
df3 <- df3[!(df3$Destination=="LAFY"),]
df3 <- df3[!(df3$Destination=="ORIN"),]
df3 <- df3[!(df3$Destination=="ROCK"),]
df3 <- df3[!(df3$Destination=="LAKE"),]
df3 <- df3[!(df3$Destination=="FTVL"),]
df3 <- df3[!(df3$Destination=="COLS"),]
df3 <- df3[!(df3$Destination=="SANL"),]
df3 <- df3[!(df3$Destination=="BAYF"),]
df3 <- df3[!(df3$Destination=="CAST"),]
df3 <- df3[!(df3$Destination=="WDUB"),]
df3 <- df3[!(df3$Destination=="DUBL"),]
df3 <- df3[!(df3$Destination=="HAYW"),]
df3 <- df3[!(df3$Destination=="SHAY"),]
df3 <- df3[!(df3$Destination=="UCTY"),]
df3 <- df3[!(df3$Destination=="FRMT"),]
df3 <- df3[!(df3$Destination=="WSPR"),]

df3 <- df3 %>% group_by(Origin, Destination, Time) %>%
  summarise_each(funs(sum))

df3 <- df3[!(df3$Time=="21:00:00"),]
df3 <- df3[!(df3$Time=="22:00:00"),]
df3 <- df3[!(df3$Time=="23:00:00"),]
df3 <- df3[!(df3$Time=="00:00:00"),]
df3 <- df3[!(df3$Time=="01:00:00"),]
df3 <- df3[!(df3$Time=="02:00:00"),]
df3 <- df3[!(df3$Time=="03:00:00"),]
df3 <- df3[!(df3$Time=="04:00:00"),]
df3 <- df3[!(df3$Time=="05:00:00"),]
## 3. When is the best time to go to SF if you want to find a seat? (I removed 9PM-5AM) 
##It really depends what station you are leaving from, but you should definitely avoid 7AM to 10AM.

df2016$Weekday <- ordered(df2016$Weekday, levels=c("Monday", 
                                                   "Tuesday", 
                                                   "Wednesday", 
                                                   "Thursday", 
                                                   "Friday", 
                                                   "Saturday", 
                                                   "Sunday"))
barplot(table(df2016$Weekday))

df2017$Weekday <- ordered(df2017$Weekday, levels=c("Monday", 
                                                   "Tuesday", 
                                                   "Wednesday", 
                                                   "Thursday", 
                                                   "Friday", 
                                                   "Saturday", 
                                                   "Sunday"))
barplot(table(df2017$Weekday))
## 4.Which days of the week are the busiest in 2016 and 2017? 2016=Friday; 2017=Wednesday

Time2016 <- table(df2016$Time)
View(Time2016)
latenight2016 <- c(444220 + 377757 + 272661 + 87784)
latenight2016

Time2017 <- table(df2017$Time)
View(Time2017)
latenight2017 <- c(144600 + 120774 + 84049 + 26592)
latenight2017
## 5.How many people take the BART late at night(10PM-1AM)? 2016 = 1,182,422; 2017 = 376,015

which(df2016$Throughput == 0)
which(df2017$Throughput == 0)
## 6.Does the BART ever stop in a station without anyone going off or on? Answer=No
###Note: Throughput is defined as 'Number of passengers that went between two stations'.






