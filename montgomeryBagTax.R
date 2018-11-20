#Written by Daphne Basangwa 10/22/2018
#This program may be used and distributed for educational and personal purposes only

#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("cowplot")
library(cowplot)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

#1. Read populatin csv file.
pop <- read.table("population.txt", sep="\t", header=TRUE)

popPlot <- ggplot(data = pop, aes(x = year, y = population/(1*10^6))) +
  geom_point(size = 2) +
  geom_smooth(method = loess, formula = y~x, se = FALSE, aes(colour = "Loess Curve")) +
  geom_smooth(method = lm, formula = y~x, se = FALSE, aes(colour = "Linear Model")) + 
  xlab("Year") + ylab("Montgomery County Population [Millions]") +
  scale_colour_manual(name="", values=c("#E69F00", "#999999")) + 
  theme_bw()
popPlot

#2. Read zipcode file.
zip <- read.delim("zipcodes.txt")

#3. Read Montgomery County Bag Tax file. Remove unused columns and rename columns
dat = read.csv("Bag_Tax.csv", stringsAsFactors=FALSE)
dat = subset(dat, select = c("Account","Date.From","Date.To","Create.Date","Bag.Count","Vendor.Name","City","State","Zip.code"))
names(dat) <- c("id","from","to","created","bagCount","vendorName","city","state","zipCode")

#Reformatting date columns from text to Date objects
dat$from = as.Date(dat$from, format="%m/%d/%Y")
dat$to = as.Date(dat$to, format = "%m/%d/%Y")
dat$created = as.Date(dat$created, format = "%m/%d/%Y")

#Removing all data with incorrect Dates 
#Removing all data where from date is after to date
dat <- subset(dat,to > from)
#Removing all data not bound by legislation date and date of last update of data
legislationDate = as.Date("5/3/2011",format = "%m/%d/%Y")
dataUpdateDate = as.Date("9/9/2018",format = "%m/%d/%Y")
dat <- subset(dat, from >= legislationDate)
dat <- subset(dat, to <= dataUpdateDate)

#Convert bagcount to units of millions
dat$bagCount <- dat$bagCount/(1*10^6)

#Assign Variable for location.
dat$location = "Nonlocal"
for (row in 1:nrow(dat)) {
  if (dat[row,"zipCode"] %in% zip$localCodes){
    dat[row,"location"] <- "Local"
  }
  else if (dat[row,"state"] == "MD") { 
    dat[row,"location"] <- "Semi-Local"   
  }
}

dat$location <- factor(dat$location, levels = c("Local","Semi-Local","Nonlocal"))
confetti <- ggplot(dat, aes(x=from, y=bagCount,colour = location)) +
  geom_point(size = 1.2) +
  xlab("Year") + ylab("Number of Bags Sold [Millions]") + 
  labs(colour = "Store HQ Location") +
  scale_color_manual(values = c("#00BFCA","#000000","#F8766D")) +
  theme_bw() 
confetti

#Read months file
months = read.delim("months.txt")
months$firstDay = as.Date(months$firstDay, format = "%m/%d/%Y")
months$lastDay = as.Date(months$lastDay, format = "%m/%d/%Y")

#initiate months columns. 
months$bagCount = 0;
months$localCount = 0;
months$semiLocalCount = 0;
months$nonlocalCount = 0;

#Proportion out bagCount into months
#Come back to make these loops more efficient.
fraction = 0
start_time <- Sys.time()
for (row in 1:nrow(months)) {
  a <- as.numeric(months[row, "firstDay"])
  b <- as.numeric(months[row, "lastDay"])
  
  for (row2 in 1:nrow(dat)){
    c <- as.numeric(dat[row2, "from"])
    d <- as.numeric(dat[row2, "to"])
    count = dat[row2,"bagCount"]
    
    if (d<a | b<c){ # order c d a b or a b c d
      next
    }
    else if (a<=c & c<=b & b<=d) { #order a c b d
      fraction <- (b-c+1)/(d-c+1)
    }
    else if (a<=c & c<=b & d<=b) { #order a c d b
      fraction = 1
    }
    else if (a<=d & d<=b & c<=a) { #order c a d b
      fraction = (d-a+1)/(d-c+1)
    }
    else {                        #order c a b d 
      fraction = (b-a+1)/(d-c+1)
    }
    
    if (dat[row2,"location"] == "Local"){
      months[row, "localCount"] = months[row, "localCount"] + fraction*count
    }
    else if(dat[row2,"location"] == "Semi-Local"){
      months[row, "semiLocalCount"] = months[row, "semiLocalCount"] + fraction*count
    }
    else{
      months[row, "nonlocalCount"] = months[row, "nonlocalCount"] + fraction*count
    }
  }
}
months$bagCount = months$localCount + months$semiLocalCount+months$nonlocalCount
end_time <- Sys.time()
end_time-start_time

#We'll do 2012 - 2018
dayFirst = as.Date("2012-01-01")
dayLast = as.Date("2018-06-01")
months <- subset(months, firstDay >= dayFirst & firstDay <= dayLast )

####
scatter <- ggplot(data = months, aes(x =firstDay, y=bagCount)) +
  geom_point() +
  xlab("Time") + ylab("Number of Bags Sold [Millions]") +
  theme_bw() 
scatter

####
oneLine <- ggplot(data = months, aes(x = firstDay, y = bagCount)) +
  geom_line() +
  xlab("Time") + ylab("Number of Bags Sold [Millions]") +
  theme_bw() 
oneLine

####
threeLines <- ggplot(data = months, aes(x =firstDay)) +
  geom_line(aes(y = localCount, colour = "Local")) +
  geom_line(aes(y = nonlocalCount, colour = "Non-Local")) +
  geom_line(aes(y = semiLocalCount, colour = "Semi-Local")) +
  
  scale_colour_manual(breaks = c( "Local", "Semi-Local","Non-Local"),
                      values = c("#F8766D","#00BFCA","#000000")) +
  xlab("Year") + ylab("Number of Bags Sold [Millions]") +
  theme(legend.title=NULL) +
  labs(colour = "Store HQ Location") +
  theme_bw() 
threeLines
####

months$month = months(months$firstDay)
months$year = as.numeric(format(months$firstDay,"%Y"))
months$month <- factor(months$month, levels=unique(months$month))
#Compute the marketshare for non-local stores by percentage of bags sold
months$nlMarketShare <- months$nonlocalCount/months$bagCount

jaggedMonths <- ggplot(data = months, aes(x=month, y=bagCount, group=year), xlab="Month", ylab="Bags Sold (Millions)") +
  geom_line(aes(color=year)) +
  geom_point(aes(color=year)) +
  xlab("Month") + ylab("Number of Bags Sold [Millions]") +
  labs(colour = "Year") +
  ylim(4,7) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) 


growthCurve <- ggplot(filter(months,year %in% c(2012:2017)), aes(x=year, y=bagCount, colour=month, group=month)) +
  geom_line(size = 1) + 
  xlab("Year") + ylab("") +
  ylim(4,7) +
  labs(colour = "Month") +
  theme_bw() 
plot_grid(jaggedMonths, growthCurve, labels = "AUTO", align = "h")

sinusoidal <- ggplot(months, aes(firstDay,nlMarketShare)) +
  geom_point() +
  geom_smooth(method = loess) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  xlab("Time") + ylab("Market Share")
sinusoidal

years = data.frame(yr = seq(2012,2017))
years$bagCount = 0
years$bagCount = sum(months$bagCount)
for (row in 1:nrow(years)){
  annee = years[row,"yr"] #annee is French for "year" baby!
  years[row,"bagCount"] = sum(filter(months,year == annee)$bagCount)
}

years$pop = pop[3:nrow(pop),"population"]
years$bagsPerCapita = years$bagCount/years$pop

for (row in 1:nrow(years)){
  year <- years[row,"yr"]
  df <- subset(dat,as.numeric(format(from,"%Y")) == year | as.numeric(format(to,"%Y")) == year)
  years[row,"nVendors"] = length(unique(df$id))
  #max(sum(sapply(dat$from, function(x) grepl(year,x))),sum(sapply(dat$to, function(x) grepl(year,x))))
}

for (row in 1:nrow(years_long)){
  year <- years_long[row,"ye"]
  df <- subset(dat,as.numeric(format(from,"%Y")) == year | as.numeric(format(to,"%Y")) == year)
  years_long[row,"nVendors"] = length(unique(df$id))
  #max(sum(sapply(dat$from, function(x) grepl(year,x))),sum(sapply(dat$to, function(x) grepl(year,x))))
}


myears <- lm(bagCount ~ yr, data = years) 
summary(myears)

myears2 <- lm(bagCount ~ yr+pop, data = years) 
summary(myears2)

myears3 <- lm(bagCount*(1*10^6) ~ pop, data = years)
summary(myears3)

myears4 <- lm(bagsPerCapita ~ yr, data = years)
summary(myears4)

### Year regressionl line
yearLM <- ggplot(data = years, aes(x = yr, y = bagCount)) + 
  geom_point() +
  geom_smooth(method = "lm", aes(colour = "Model 1"), se = FALSE) + 
  scale_colour_manual(name="", values=c("#F8766D")) + 
  xlab("Year") + ylab("Number of Bags Sold [Millions]") +
  ylim(50, 70) + 
  theme_bw()
yearLM

##
popLM <- ggplot(data = years, aes(x = pop/(1*10^6), y = bagCount)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "#E69F00", se = FALSE) + 
  xlab("Population [Millions]") + ylab("Number of Bags Sold [Millions]") +
  ylim(50, 70) + 
  theme_bw()
popLM
##

#Residual Plot. Not very informative of course - sparce data!
resPlot <- ggplot(myears, aes(x=.fitted, y=.resid)) + 
  geom_point(aes(x=.fitted, y=.resid)) + 
  geom_hline(yintercept=0) + 
  theme_bw()
resPlot

vendors = data.frame(name = unique(dat$vendorName))
vendors$bagCount <- 0
vendors$id <- 0 
vendors$location <- "unAssigned"

for (row in 1:nrow(vendors)){
  vendName = vendors[row,"name"]
  vendors[row,"bagCount"] <- sum(dat[dat$vendorName == vendName,]$bagCount)
  vendors[row,"id"] <- dat[match(vendName,dat$vendorName),"id"]
  vendors[row,"location"] <- dat[match(vendName,dat$vendorName),"location"]
}
#reorder vendors dataframe with largest by bagCount at the top
vendors <- vendors[order(-vendors$bagCount),]

#Set order for plotting vendors bar graph
vendors$name <- factor(vendors$name, levels = vendors$name[order(vendors$bagCount)])

vendorBar <- ggplot(data=head(vendors,30), aes(x=name, y=bagCount,fill = as.factor(location))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("Bags Sold Between 2012 and 2017 [Millions]") +
  labs(fill= "Store HQ Location") +
  theme_bw() 
vendorBar
### This is where the code ends for Post 1.

#wic for "what is create?"
#Exploring if "created" is the date of creation of the vendor account, or the entry (row).
#This plot shows that it's the latter, as the "to" date almost always precedes the "create" date
wic <- ggplot(data = dat, aes(x = to, y = created)) + 
  geom_point() +
  theme_bw()
wic

dat <- dat[order(dat$from),] 
dat_r <- dat[order(-as.numeric(dat$from)),] #dat_r for dat_reversed

uniqueID <- data.frame(id = unique(dat$id))

for (row in 1:nrow(uniqueID)){
  identifier = uniqueID[row,"id"]
  uniqueID[row,"opened"] <- dat[match(identifier,dat$id),"from"]
  uniqueID[row,"last"] <- dat[match(identifier,dat_r$id),"to"]
  uniqueID[row,"name"] <- dat[match(identifier,dat_r$id),"vendorName"]
}

uniqueID <- uniqueID[order(uniqueID$opened),]
uniqueID$N <- seq(1:nrow(uniqueID))
cumulative <- ggplot(data = uniqueID) + 
  geom_line(data = uniqueID, aes(x = opened, y = N), colour = "red")
#geom_line(data = uniqueID_r, aes(x = last, y = N), colour = "green")
cumulative

uniqueID <- uniqueID[order(uniqueID$last),]

uniqueID_r <- uniqueID[order(as.numeric(uniqueID$last)),]
uniqueID_r$N <- seq(1:nrow(uniqueID))
attrition <- ggplot(data = uniqueID_r, aes(x = last, y = N)) + 
  geom_point()
#xlim(dayFirst,dayLast)
attrition

