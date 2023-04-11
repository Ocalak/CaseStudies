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
mean(dataset_QD$BTC_gr)
var(dataset_QD$BTC_gr)

SP_p <- adf.test(dataset_QD$SP500_gr)$p.value
BTC_p <- adf.test(dataset_QD$BTC_gr)$p.value
UNEMP_p <- adf.test(dataset_QD$UNEMP)$p.value
FEDF_P <- adf.test(dataset_QD$FEDF)$p.value
INFR_p <- adf.test(dataset_QD$INFR)$p.value
#Here we reject h null if th p value is smaller than 0.05 then serie is stationary. Otherwise it is not.



kk <- diff(dataset_QD$FEDF,differences = 1,lag=1)
kk <-  ma(dataset_QD$FEDF, order = 8)


plot(dataset_QD$FEDF,type="l")
Pacf(dataset_QD$FEDF,lag.max = 50)
acf(dataset_QD$FEDF)


 

bitcoin_ts <- as.data.frame(bitcoin_ts)



normalize <- function(data){
  data <- (data-mean(data))/sd(data)
  return(data)
}




#bitcoin_ts <- ts(scale(dataset_QD[c("BTC_gr","SP500_gr","UNEMP","FEDF","INFR")]), frequency = 4, start = c(2010,3))
######
bitcoin_ts <- ts(dataset_QD[c("BTC_gr","SP500_gr","UNEMP","FEDF","INFR")], frequency = 4, start = c(2010,3))

normalize <- function(data){
  data <- (data-mean(data))/sd(data)
  return(data)
}
for (i in 1:5) {
  bitcoin_ts[,i] <- normalize(bitcoin_ts[,i])
}

bitcoin_ts <- as.data.frame(bitcoin_ts)


ggplot(bitcoin_ts, aes(x=DATE)) + 
  geom_line(aes(y = BTC_gr), color = "darkred") + 
  geom_line(aes(y = UNEMP), color="steelblue") +
  geom_line(aes(y = INFR), color="green")+
  geom_line(aes(y = FEDF), color="black") +
  geom_line(aes(y = SP500_gr), color="purple")

bitcoin_ts <- ts(bitcoin_ts, frequency = 4, start = c(2010,3))

#####
# Step 2: Split data into training and test sets
train <- window(bitcoin_ts, end = c(2022,3))
test <- window(bitcoin_ts, start = c(2022,4))

# Step 3: Fit AR(1) model and generate forecasts
ar1_model <- arima(train[,1], order = c(1,0,0))
ar1_forecast <- forecast(ar1_model, h = 1)

# Step 4: Calculate root mean squared forecasting error
rmse <- sqrt(mean((ar1_forecast$mean - test)^2))

# Step 5: Plot actual and forecasted growth rates
plot(bitcoin_ts[,1], main = "Bitcoin Growth Rates")
lines(ar1_forecast$mean, col = "red")
points(ar1_forecast$mean,col="blue",bg="red",lwd=1)

text(x = c(2019,2), y = -0.6, col="red",labels = paste0("RMSE: ", round(rmse, 1)))




#------q1-D)qqqqquestion



# Convert the data into a time series format
data.ts <- ts(data, start=c(2010,3), frequency=4)

# Split the data into training and test sets
train.ts <- window(bitcoin_ts, end=c(2019,1))
test.ts <- window(bitcoin_ts, start=c(2019,2))

# Estimate the VAR(1) model using the training set
var.model <- VAR(train, p=1)

# Generate one-quarter-ahead forecasts for all the variables using the VAR(1) model
var.forecast <- predict(var.model, n.ahead=1, ci=0.95, newdata=test)

# Extract the forecasted Bitcoin growth rate
bitcoin.forecast.var <- var.forecast$fcst[1][[1]][,1]
#ts_ts <- ts(bitcoin.forecast.var, start=c(2019,4), frequency=4)
# Compare the forecasting accuracy of the VAR(1) model to that of the AR(1) model
ar1.model <- arima(train[,1], order=c(1,0,0))
ar1.forecast <- predict(ar1.model, n.ahead=1, interval="predict", level=0.95)
bitcoin.forecast.ar1 <- ar1.forecast$pred
rmse.var <- sqrt(mean((bitcoin.forecast.var - test[,1])^2))
rmse.ar1 <- sqrt(mean((bitcoin.forecast.ar1 - test[,1])^2))

# Visualize the results
plot(test[,1], type="l", col="blue", ylim=c(-0.8,0.5), xlab="Year", ylab="Bitcoin growth rate")
points(bitcoin.forecast.ar1, col="red")
bitcoin.forecast.var <- ts(bitcoin.forecast.var,start = c(2019,4),frequency = 4)
points(bitcoin.forecast.var, col="green")

legend("topleft", c("Actual", "AR(1) Forecast", "VAR(1) Forecast"), col=c("blue", "red", "green"), lty=1)
text(2022, 0.0, paste("RMSE (VAR) =", round(rmse.var, 1)))
text(2022, 0.1, paste("RMSE (AR(1)) =", round(rmse.ar1, 1)))







#---------------
#f
var.select <- VARselect(bitcoin_ts, lag.max = 10, type = "const")
p <- var.select$selection["AIC(n)"]
cat("Selected VAR lag order: ", p, "\n")

model <- VAR(bitcoin_ts, p)

# Make one-quarter-ahead forecasts
fcst <- predict(model, n.ahead = 1)

# Calculate root mean squared forecasting error

accuracy(fcst$fcst[[1]][,1], bitcoin_ts[,1][2])

plot(cbind(bitcoin_ts[,1], fcst$fcst[[1]][,1]), type="l", 
     col=c("black", "red", "blue"), lty=c(1,2,2), ylim=c(-0.5,0.5), 
     main="Actual and forecasted Bitcoin growth rates", xlab="Year", ylab="Growth rate")
legend("topright", legend=c("Actual", "AR(1)", paste("VAR(", p, ")")), 
       col=c("black", "red", "blue"), lty=c(1,2,2))


