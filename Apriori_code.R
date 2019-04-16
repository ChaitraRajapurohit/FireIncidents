library(e1071)
library(lubridate)
library(tidyverse)
library(dplyr)
library(zoo) #for seasons
library(chron) #for timeperiods
library(plotly)
library(arules)
library(RColorBrewer)
#library(robustbase)
library(arulesViz)
#Setting the working directory
setwd('/Users/chaitra/Desktop/Capstone')
getwd()

#Step 1 : Collecting the data
#reading the csv file
firx <- read.csv("Fireincidents.csv",stringsAsFactors = FALSE)
#converting incidentdate variable to 'Date' type and filtering
firx$IncidentDate <- as.Date(firx$IncidentDate)
firx <- dplyr::filter(firx, IncidentDate >= '2018-01-01' & IncidentDate <= '2018-12-31')
str(firx)
fir_x <- select(firx,IncidentDate,AlarmDtTm,Zipcode,PropertyUse,Battalion,
                NeighborhoodDistrict)
str(fir_x)

#Calculating the Season based on months
mth <- as.yearqtr(as.yearmon(fir_x$IncidentDate, "%m/%d/%y") + 1/12)
fir_x$Season <- factor(format(mth, "%q"), levels = 1:4, 
                        labels = c("Winter","Spring","Summer","Fall"))
table(fir_x$Season)

#Converting alarmdttm variable from 'char' to 'time' variable
fir_x$AlarmDtTm<- as.POSIXct(fir_x$AlarmDtTm, format = "%H:%M") %>% format("%H:%M:%S")
#Calculating the time_period based on 3-hr time frame
fir_x$Time_period <- cut(chron::times(fir_x$AlarmDtTm) , breaks = (1/24) * c(0,3,6,9,12,15,18,21,24), 
                          labels = c("Early-Night","Late-Night","Early-Morning","Late-Morning", "Early-Afternoon","Late-Afternoon", "Early-Evening","Late-Evening"))
table(fir_x$Time_period)

#Selecting the required columns and writing it into a .csv file
fir_x <- select(fir_x,Zipcode,Season,Time_period,PropertyUse,Battalion,
                NeighborhoodDistrict)
#write.csv(fir_x,file = "display.csv",row.names = FALSE)
write.csv(fir_x,file = "display1.csv",row.names = FALSE)
dsply <- read.csv("display1.csv")

#BarPlot for Zipcode with accidents
bx <- as.data.frame(table(dsply$Zipcode)) #Converting non-numeric to table with freq
bx_order <- bx %>% rename(Zipcodes = Var1) %>% arrange(desc(Freq)) 
print(paste('The Zipcode with most accidents is:',bx_order[1,1],'with a frequency of',bx_order[1,2]))
bx_order <- as.data.frame(bx_order)
zip_plotly <- plot_ly(bx_order,x=~Zipcodes,y=~Freq,type = 'bar',
                      marker = list(color = 'navy', line = list(color = 'black',width = 2)))
zip_plotly

#Barplot for Season with accidents
Ssn <- as.data.frame(table(dsply$Season)) 
Ssn <- Ssn %>% rename(season = Var1) %>% arrange(desc(Freq))
print(paste('The Season with most accidents is:',Ssn[1,1],'with a frequency of',Ssn[1,2]))
Ssn_plotly <- plot_ly(Ssn,x=~season,y=~Freq,type = 'bar',
                      marker = list(color = 'silver', line = list(color = 'black',width = 2)))
Ssn_plotly 
#Barplot for Property with accidents
Prop <- as.data.frame(table(dsply$PropertyUse))
Prop <- Prop %>% arrange(desc(Freq)) %>% rename(property = Var1)
print(paste('The Property with most accidents is:',Prop[1,1],'with a frequency of',Prop[1,2]))
Prop_plotly <- plot_ly(Prop,x=~property,y=~Freq,type = 'bar',
                       marker = list(color = 'red', line = list(color = 'black',width = 1)))
Prop_plotly

#Barplot for Neighborhood District with Accidents
ND <- as.data.frame(table(dsply$NeighborhoodDistrict))
ND <- ND %>% arrange(desc(Freq)) %>% rename(neighborhood = Var1)
print(paste('The Neighborhood with most accidents is:',ND[1,1],'with a frequency of',ND[1,2]))
ND_plotly <- plot_ly(ND,x=~neighborhood,y=~Freq,type = 'bar',
                       marker = list(color = 'teal', line = list(color = 'black',width = 2)))
ND_plotly

#Barplot for Timeperiods of Accidents
TP <- as.data.frame(table(dsply$Time_period)) %>% rename(Timeperiod = Var1) %>% arrange(desc(Freq))
print(paste('The Time_period with most accidents is:',TP[1,1],'with a frequency of',TP[1,2]))
TP_plotly <- plot_ly(TP,x=~Timeperiod,y=~Freq,type = 'bar',
                     marker = list(color = 'rgb(199,21,133)', line = list(color = 'black',width = 2)))
TP_plotly

#Barplot for Battalion
Batt <- as.data.frame(table(dsply$Battalion)) %>% rename(battalion = Var1) %>% arrange(desc(Freq))
print(paste('The Battalion with maximum first responses is:',Batt[1,1],'with a frequency of',Batt[1,2]))
Batt_plotly <- plot_ly(Batt,x=~battalion,y=~Freq,type = 'bar',
                       marker = list(color = 'indigo', line = list(color = 'black',width = 2)))
Batt_plotly

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(dsply, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup5[i] <- length(apriori(dsply, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(dsply, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup0.5[i] <- length(apriori(dsply, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}

# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  theme_bw()
#install.packages('gridExtra')
library('gridExtra')
library('ggplot2')
# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +

# Plot line and points (support level of 10%)
geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
# Plot line and points (support level of 5%)
geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
# Plot line and points (support level of 1%)
geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
# Plot line and points (support level of 0.5%)
geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
# Labs and theme
labs(x="Confidence levels", y="Number of rules found", 
    title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

#Writing a csv file for analysis
#write.csv(fir_x, file = "firx.csv",row.names = FALSE)
firy <- read.transactions("display1.csv", sep = ",",format = c("basket"),skip = 1)
summary(firy)
inspect(firy[1:5]) #Reading the first 5 rows of the sparse matrix
itemFrequency(firy[,1:5]) #support of the first 5 measures
arules::itemFrequencyPlot(firy,topN=15,col=brewer.pal(8,'Pastel2'),main='Relative variable Frequency Plot',
                          type="relative",ylab="Variable Frequency (Relative)")
image(firy[1:20]) #plotting sparse matrix for 20 rows and 230 columns
image(sample(firy,200)) #plotting sparse matrix for 200 rows and 230 columns

#training aprori model on the data
firyrules<- apriori(firy,parameter = list(support = 0.01,confidence = 0.25))
firyrules
summary(firyrules)
inspect(firyrules[1:20])

#improving the model by sorting and subsetting
inspect(sort(firyrules, by = "lift")[1:8])
improv <- subset(firyrules,items %ain% "B03") #subset for frequent Battalion
inspect(sort(improv,by = "lift")[1:20])

#Vizualising the association rules
#graph of rules for Battalion B03
plot(improv[1:20],
     method = "graph",
     control = list(type = "items"))

improv1 <- subset(firyrules,items %ain% "94103") 
inspect(sort(improv1,by = "lift")[1:20])

#graph of rules for zipcode 94103
plot(improv1[1:20],
     method = "graph",
     control = list(type = "items"))

improv2 <- subset(firyrules,items %ain% "Winter")
inspect(sort(improv2,by = "lift")[1:20])

#graph of rules for winter season
plot(improv2[1:20],
     method = "graph",
     control = list(type = "items"))

improv3 <- subset(firyrules,items %ain% "429 - multifamily dwellings")
inspect(sort(improv3,by = "lift")[1:20])

#graph of rules for multifamily dwellings property
plot(improv3[1:20],
     method = "graph",
     control = list(type = "items"))

improv4 <- subset(firyrules,items %ain% "Tenderloin")
inspect(sort(improv4,by = "lift")[1:20])

#graph of rules for Tenderloin neighborhood
plot(improv4[1:20],
     method = "graph",
     control = list(type = "items"))

improv5 <- subset(firyrules,items %ain% "Late-Afternoon")
inspect(sort(improv5,by = "lift"))

#graph of rules for Late afternoon time period
plot(improv5,
     method = "graph",
     control = list(type = "items"))
