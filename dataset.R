library(lubridate)
library(dplyr)
library(quantmod)
library(readr)
library(EnvStats)
library(tidyverse)
library(readxl)
library(forecast)

library(vars)
library(tseries)

#quantmod::getSymbols(c("BTC-USD", "^GSPC"))

#Source is FRED_QD from the Exercise paper.
data1 <- read.csv("~/Downloads/current.csv")
#fredqd(file="~/Downloads/current.csv",date_start = NULL,date_end = NULL,transform = FALSE)






SP500 <- data1$S.P.500#It needs to be converted to growth rate as well.
UNEMP <- data1$UNRATE#Percentage
FEDF <- data1$FEDFUNDS#percentage



#Or we can use
#quantmod::getSymbols("BTC-USD")
#btc_qd <- to.quarterly(`BTC-USD`)


#BTC data from coin market capital (weekly)!!!
btc_weekly <- read_delim("~/Downloads/btc_qd.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
btc_weekly$date <- as.Date(btc_weekly$timestamp,format="%m%d%y")

btc_weekly <- data_frame(btc_weekly$date,btc_weekly$close)
colnames(btc_weekly) <- c("date","price")


btc_weekly$quarter <- quarter(btc_weekly$date, with_year = TRUE)
btc_quarter_labels <- btc_weekly %>%
  group_by(quarter)%>%
  summarise(price)



btc_qd <- list()#Create an empty list to store dates(quarters and prices)
btc_att_qd <- attributes(btc_quarter_labels)[4][1]#Save the quarters

#Lets create quarterly ordered the dates column into the new list.
btc_qd$date <- btc_att_qd$groups$quarter
#Lets create price column
btc_qd$price <- 0

#Lets define first observation of he columns by calculating the geometrics mean
#geometrics mean for the 2010.3 
#Let me explain the formula here. Btw, same formula is going to be used for the calculate the geometric means for other quarters as well.

btc_qd$price[1] <- geoMean(btc_weekly$price[1:btc_att_qd$groups$.rows[[1]][[length(btc_att_qd$groups$.rows[[1]])]]])

#This part of the code to get the number of the weeks in the same quarter of the year  2010.
# Here in this data we have 13 weeks for 3rd quarter of 2010.
btc_att_qd$groups$.rows[[1]][[length(btc_att_qd$groups$.rows[[1]])]]#=13
#Basically we are just calculating the geometric mean for price[1:13]
######There might be an easy way to compute(less complicated) it but thats the first things that
#I remember :D 


#We have the formula to calculate the geometric mean for 1 quarter.

#Now, I implement a loop to do it for other quarters too;

for (i in 1:50){
  btc_qd$price[i+1] <-geoMean(btc_weekly$price[btc_att_qd$groups$.rows[[i]][[length(btc_att_qd$groups$.rows[[i]])]]:btc_att_qd$groups$.rows[[i+1]][[length(btc_att_qd$groups$.rows[[i+1]])]]])
}

#Voila! :D So we have qeometric means for all the quarters from 2010.3 to 2023.1
print(btc_qd)

#Now we have the quarterly data for bitcoin.

#I already get the quarterly data for SP500&UNEMP&FEDF.

index = 209:258
#Index  is 209:259 means that in the dataset$date we have the take the rows  209 and 258.
#which is the data from 2010.Q3 to 2022.Q4
#We will use that later.


#We have FEDF(perc) SP500(price) UNEMP(perc) BTC(price)
#####Now Inflation rate. Link: https://fred.stlouisfed.org
INFR <- read_xls("~/Downloads/CPI.xls")$CPI #The last observation is for 2022.Q4
#first one is 2010.Q3


length(INFR)#50 Becuase we get 2022.Q4 nothing from 2023
#Now lets picked the rows from 2010.Q3 to 2022.Q4 for BTC, UNEMP,FEDF,SP500.
#We did it already for Inflation rate
#Remember we set the index as 208:258;



SP500 <- SP500[209:258]
UNEMP <- UNEMP[209:258]
FEDF <- FEDF[209:258]
DATE <- btc_qd$date[1:50]
BTC <- btc_qd$price[1:50]


#Lets gather all the variables in a data frame and save it!

dataset_QD <- data_frame(DATE,BTC,UNEMP,FEDF,INFR,SP500)

write.csv(dataset_QD, "~/Desktop/CaseStudies/dataset_QD.csv", row.names=FALSE)


#Now, We are asked to use growth rate of BTC but we also need growth rate of SP500.
#The one that we have is just the price of it.

#I implement a function to calcualte growth rate of the variables BTC and SP500
#Basically I just set the first obs. to 0

growrth <- function(x){
  x <- c(0,(x[-1]-x[-length(x)])/x[-length(x)])
  x <- x*100
  return(x)
}
dataset_QD$BTC_gr <- growrth(BTC)
dataset_QD$SP500_gr <- growrth(SP500)
