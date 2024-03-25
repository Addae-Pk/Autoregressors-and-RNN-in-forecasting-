library(ggplot2)
library(dplyr)
library(moments)
library(EnvStats)
library(tsoutliers)
library(mFilter)
library(tseries)
library(forecast)
library(urca)
library(vars)
library(systemfit)
library(corrplot)
library(MASS)

####data######
###GDP###
gdp <- read.csv("USGDP.csv")
gdp <- gdp[,2]

l_gdp <- log(gdp)
ld_gdp <- diff(l_gdp)
dld_gdp <- diff(ld_gdp)

#Outliers
jarque.bera.test(gdp)

jarque.bera.test(ld_gdp)
Boxplot(ld_gdp)
boxplot.stats(ld_gdp)
outliers_date <- Boxplot(ld_gdp)
ld_gdp[outliers_date] <- mean(ld_gdp)
plot(ld_gdp, type = 'l')
jarque.bera.test(ld_gdp)

jarque.bera.test(dld_gdp)
Boxplot(dld_gdp)
boxplot.stats(dld_gdp)
outliers_date <- Boxplot(dld_gdp)
dld_gdp[outliers_date] <- mean(dld_gdp)
plot(dld_gdp, type = 'l')
jarque.bera.test(dld_gdp)

###Stationary
plot(gdp, type = 'l', main = 'GDP', xlab = 'Time')
acf(gdp)

plot(ld_gdp, type = 'l', main = 'Log-diff of GDP', xlab = 'Time')
acf(ld_gdp, main = 'ACF of the log-diff of GDP')
kpss.test(ld_gdp)
adf.test(ld_gdp)
plot(bkfilter(ld_gdp))
GDP_hp <- hpfilter(ld_gdp, freq = 1600)
plot(GDP_hp)

plot(dld_gdp, type = 'l', main = 'Second difference of the log of GDP', xlab = 'Time')
acf(dld_gdp)
kpss.test(dld_gdp)
adf.test(dld_gdp)
plot(bkfilter(dld_gdp))
GDP_hp <- hpfilter(dld_gdp, freq = 1600)
plot(GDP_hp)
summary(ur.df(dld_gdp, type = "trend"))

real_values_gdp <- dld_gdp[80:91]
dld_gdp <- dld_gdp[1:79]

print(dld_gdp)

###PPI###
ppi <- read.csv("PPIACO.csv")
ppi$DATE <- as.Date(ppi$DATE)
rownames(ppi) <- ppi[, 1]
ppi <- ppi[-1]

l_ppi <- log(ppi)
ld_ppi <- diff(l_ppi[,1])
ld_ppi <- data.frame(Log_Differences = c(ld_ppi))
rownames(ld_ppi) <- rownames(ppi)[-1]

#Outliers
jarque.bera.test(ppi[,1])

jarque.bera.test(ld_ppi[,1])
boxplot(ld_ppi)
boxplot.stats(ld_ppi$Log_Differences)
outliers_date <- Boxplot(ld_ppi)
ld_ppi[outliers_date,] <- mean(ld_ppi[,1])
plot(ld_ppi[,1], type = 'l')
jarque.bera.test(ld_ppi[,1])

#Stationary
plot(l_ppi[,1], type = 'l')
acf(l_ppi)

plot(ld_ppi[,1], type = 'l')
acf(ld_ppi[,1])
kpss.test(ld_ppi[,1])
adf.test(ld_ppi[,1])

real_values_eu <- ld_ppi[81:92, 1]
ld_ppi <- ld_ppi[2:80, 1]

###EURO###
eu <- read.csv("DEXUSEU.csv")
eu$DATE <- as.Date(eu$DATE)
rownames(eu) <- eu[, 1]
eu <- eu[-1]

l_eu <- log(eu)
ld_eu <- diff(l_eu[,1])
ld_eu <- data.frame(Log_Differences = c(ld_eu))
rownames(ld_eu) <- rownames(eu)[-1]

#Outliers
jarque.bera.test(eu[,1])

jarque.bera.test(ld_eu[,1])
boxplot(ld_eu)
boxplot.stats(ld_eu$Log_Differences)
outliers_date <- Boxplot(ld_eu)
ld_eu[outliers_date,] <- mean(ld_eu[,1])
plot(ld_eu[,1], type = 'l')
jarque.bera.test(ld_eu[,1])

#Stationary
plot(l_eu[,1], type = 'l')
acf(l_eu)

plot(ld_eu[,1], type = 'l')
acf(ld_eu[,1])
kpss.test(ld_eu[,1])
adf.test(ld_eu[,1])

real_values_eu <- ld_eu[81:92, 1]
ld_eu <- ld_eu[2:80, 1]

###UK###
uk <- read.csv("UK.csv")
uk$DATE <- as.Date(uk$DATE)
rownames(uk) <- uk[, 1]
uk <- uk[-1]

l_uk <- log(uk)
ld_uk <- diff(l_uk[,1])
ld_uk <- data.frame(Log_Differences = c(ld_uk))
rownames(ld_uk) <- rownames(uk)[-1]

#Outliers
jarque.bera.test(uk[,1])

jarque.bera.test(ld_uk[,1])
boxplot(ld_uk)
boxplot.stats(ld_uk$Log_Differences)
outliers_date <- Boxplot(ld_uk)
ld_uk[outliers_date,] <- mean(ld_uk[,1])
plot(ld_uk[,1], type = 'l')
jarque.bera.test(ld_uk[,1])

#Stationary
plot(l_uk[,1], type = 'l')
acf(l_uk)

plot(ld_uk[,1], type = 'l')
acf(ld_uk[,1])
kpss.test(ld_uk[,1])
adf.test(ld_uk[,1])

real_values_uk <- ld_uk[81:92, 1]
ld_uk <- ld_uk[2:80, 1]

###Population###
pop <- read.csv("POPTHM.csv")
pop$DATE <- as.Date(pop$DATE)
rownames(pop) <- pop[, 1]
pop <- pop[-1]

l_pop <- log(pop)
ld_pop <- diff(l_pop[,1])
ld_pop <- data.frame(Log_Differences = c(ld_pop))
rownames(ld_pop) <- rownames(pop)[-1]

dld_pop <- diff(ld_pop[,1])
dld_pop <- data.frame(Diff_Log_Differences = c(dld_pop))
rownames(dld_pop) <- rownames(ld_pop)[-1]

#Outliers
jarque.bera.test(pop[,1])

jarque.bera.test(ld_pop[,1])
boxplot(ld_pop)
boxplot.stats(ld_pop$Log_Differences)
outliers_date <- Boxplot(ld_pop)
ld_pop[outliers_date,] <- mean(ld_pop[,1])
plot(ld_pop[,1], type = 'l')
jarque.bera.test(ld_ir[,1])

jarque.bera.test(dld_pop[,1])
boxplot(dld_pop)
boxplot.stats(dld_pop$Diff_Log_Differences)
plot(dld_pop[,1], type = 'l')

#Stationary
plot(l_pop[,1], type = 'l')
acf(l_pop)

plot(ld_pop[,1], type = 'l')
acf(ld_pop[,1])
kpss.test(ld_pop[,1])
adf.test(ld_pop[,1])

plot(dld_pop[,1], type = 'l')
acf(dld_pop)
kpss.test(dld_pop[,1])
adf.test(dld_pop[,1])

real_values_pop <- dld_pop[80:91, 1]
dld_pop <- dld_pop[1:79, 1]

###Mortgage###
mor <- read.csv("MOR.csv")
mor$DATE <- as.Date(mor$DATE)
rownames(mor) <- mor[, 1]
mor <- mor[-1]

l_mor <- log(mor)
ld_mor <- diff(l_mor[,1])
ld_mor <- data.frame(Log_Differences = c(ld_mor))
rownames(ld_mor) <- rownames(mor)[-1]

#Outliers
jarque.bera.test(mor[,1])

jarque.bera.test(ld_mor[,1])
boxplot(ld_mor)
boxplot.stats(ld_mor$Log_Differences)
outliers_date <- Boxplot(ld_mor)
ld_mor[outliers_date,] <- mean(ld_mor[,1])
plot(ld_mor[,1], type = 'l')
jarque.bera.test(ld_mor[,1])

#Stationary
plot(l_mor[,1], type = 'l', main = 'Mortgage')
acf(l_mor)

plot(ld_mor[,1], type = 'l', main = 'Log-diff of Mortgage')
acf(ld_mor, main = 'ACF of the log-diff of Mortgage')
kpss.test(ld_mor[,1])
adf.test(ld_mor[,1])

real_values_mor <- ld_mor[81:92, 1]
ld_mor <- ld_mor[2:80, 1]

###Interbank rate###
ir <- read.csv("IR.csv")
rownames(ir) <- ir[, 1]
ir <- ir[-1]

l_ir <- log(ir)
ld_ir <- diff(l_ir[,1])
ld_ir <- data.frame(Log_Differences = c(ld_ir))
rownames(ld_ir) <- rownames(ir)[-1]

dld_ir <- diff(ld_ir[,1])
dld_ir <- data.frame(Diff_Log_Differences = c(dld_ir))
rownames(dld_ir) <- rownames(ld_ir)[-1]

#Outliers
jarque.bera.test(ir[,1])

plot(ld_ir[,1], type = 'l')
jarque.bera.test(ld_ir[,1])
boxplot(ld_ir)
boxplot.stats(ld_ir$Log_Differences)
outliers_date <- Boxplot(ld_ir)
ld_ir[outliers_date,] <- mean(ld_ir[,1])
plot(ld_ir[,1], type = 'l')
jarque.bera.test(ld_ir[,1])

plot(dld_ir[,1], type = 'l')
jarque.bera.test(dld_ir[,1])
boxplot(dld_ir)
boxplot.stats(dld_ir$Diff_Log_Differences)
outliers_date <- Boxplot(dld_ir)
dld_ir[outliers_date,] <- mean(dld_ir[,1])
plot(dld_ir[,1], type = 'l')
jarque.bera.test(dld_ir[,1])

#Stationary
plot(l_ir[,1], type = 'l')
acf(l_ir)

plot(ld_ir[,1], type = 'l')
acf(ld_ir)
kpss.test(ld_ir[,1])
adf.test(ld_ir[,1])

plot(dld_ir[,1], type = 'l')
acf(dld_ir)
kpss.test(dld_ir[,1])
adf.test(dld_ir[,1])

real_values_ir <- dld_ir[, 1]
dld_ir <- dld_ir[1:79, 1]

###FGC###
fgc <- read.csv("FGC.csv")
fgc$DATE <- as.Date(fgc$DATE)
rownames(fgc) <- fgc[, 1]
fgc <- fgc[-1]

l_fgc <- log(fgc)
ld_fgc <- diff(l_fgc[,1])
ld_fgc <- data.frame(Log_Differences = c(ld_fgc))
rownames(ld_fgc) <- rownames(fgc)[-1]

dld_fgc <- diff(ld_fgc[,1])
dld_fgc <- data.frame(Diff_Log_Differences = c(dld_fgc))
rownames(dld_fgc) <- rownames(ld_fgc)[-1]

#Outliers
jarque.bera.test(fgc[,1])

jarque.bera.test(ld_fgc[,1])
boxplot(ld_fgc)
boxplot.stats(ld_fgc$Log_Differences)
outliers_date <- Boxplot(ld_fgc)
ld_fgc[outliers_date,] <- mean(ld_fgc[,1])
plot(ld_fgc[,1], type = 'l')
jarque.bera.test(ld_fgc[,1])

jarque.bera.test(dld_fgc[,1])
boxplot(dld_fgc)
boxplot.stats(dld_fgc$Diff_Log_Differences)
outliers_date <- Boxplot(dld_fgc)
dld_fgc[outliers_date,] <- mean(dld_fgc[,1])
jarque.bera.test(dld_fgc[,1])
plot(dld_fgc[,1], type = 'l')

#Stationary
plot(l_fgc[,1], type = 'l')
acf(l_fgc)

plot(ld_fgc[,1], type = 'l')
acf(ld_fgc)
kpss.test(ld_fgc[,1])
adf.test(ld_fgc[,1])

plot(dld_fgc[,1], type = 'l')
acf(dld_fgc[,1])
kpss.test(dld_fgc[,1])
adf.test(dld_fgc[,1])

real_values_fgc <- dld_fgc[80:91, 1]
dld_fgc <- dld_fgc[1:79, 1]

###Industrial production###
ipb <- read.csv("IPB.csv")
ipb$DATE <- as.Date(ipb$DATE)
rownames(ipb) <- ipb[, 1]
ipb <- ipb[-1]

l_ipb <- log(ipb)
ld_ipb <- diff(l_ipb[,1])
ld_ipb <- data.frame(Log_Differences = c(ld_ipb))
rownames(ld_ipb) <- rownames(ipb)[-1]

#Outliers
jarque.bera.test(ipb[,1])

jarque.bera.test(ld_ipb[,1])
boxplot(ld_ipb)
boxplot.stats(ld_ipb$Log_Differences)
outliers_date <- Boxplot(ld_ipb)
ld_ipb[outliers_date,] <- mean(ld_ipb[,1])
plot(ld_ipb[,1], type = 'l')
jarque.bera.test(ld_ipb[,1])

#Stationary
plot(l_ipb[,1], type = 'l')
acf(l_ipb)

plot(ld_ipb[,1], type = 'l')
acf(ld_ipb)
kpss.test(ld_ipb[,1])
adf.test(ld_ipb[,1])

real_values_ipb <- ld_ipb[81:92, 1]
ld_ipb <- ld_ipb[2:80, 1]

###IRL (risk-free rate)###
irl <- read.csv("IRL.csv")
irl$DATE <- as.Date(irl$DATE)
rownames(irl) <- irl[, 1]
irl <- irl[-1]

l_irl <- log(irl)
ld_irl <- diff(l_irl[,1])
ld_irl <- data.frame(Log_Differences = c(ld_irl))
rownames(ld_irl) <- rownames(irl)[-1]

#Outliers
jarque.bera.test(irl[,1])

jarque.bera.test(ld_irl[,1])
boxplot(ld_irl)
boxplot.stats(ld_irl$Log_Differences)
outliers_date <- Boxplot(ld_irl)
ld_irl[outliers_date,] <- mean(ld_irl[,1])
plot(ld_irl[,1], type = 'l')
jarque.bera.test(ld_irl[,1])

#Stationary
plot(l_irl[,1], type = 'l')
acf(l_irl)

plot(ld_irl[,1], type = 'l')
acf(ld_irl)
kpss.test(ld_irl[,1])
adf.test(ld_irl[,1])

real_values_irl <- ld_irl[81:92, 1]
ld_irl <- ld_irl[2:80, 1]

###Retail Trade MRT###
mrt <- read.csv("MRT.csv")
mrt$DATE <- as.Date(mrt$DATE)
rownames(mrt) <- mrt[, 1]
mrt <- mrt[-1]

l_mrt <- log(mrt)
ld_mrt <- diff(l_mrt[,1])
ld_mrt <- data.frame(Log_Differences = c(ld_mrt))
rownames(ld_mrt) <- rownames(mrt)[-1]

#Outliers
jarque.bera.test(mrt[,1])

jarque.bera.test(ld_mrt[,1])
boxplot(ld_mrt)
boxplot.stats(ld_mrt$Log_Differences)
outliers_date <- Boxplot(ld_mrt)
ld_mrt[outliers_date,] <- mean(ld_mrt[,1])
plot(ld_mrt[,1], type = 'l')
jarque.bera.test(ld_mrt[,1])

#Stationary
plot(l_mrt[,1], type = 'l')
acf(l_mrt)

plot(ld_mrt[,1], type = 'l')
acf(ld_mrt)
kpss.test(ld_mrt[,1])
adf.test(ld_mrt[,1])

real_values_mrt <- ld_mrt[81:92, 1]
ld_mrt <- ld_mrt[2:80, 1]

###S&P 500###
sp <- read.csv("S&P500.csv")
sp$Date <- as.Date(sp$Date)
rownames(sp) <- sp[, 1]
sp <- sp[-1]

l_sp <- log(sp)
ld_sp <- diff(l_sp[,1])
ld_sp <- data.frame(Log_Differences = c(ld_sp))
rownames(ld_sp) <- rownames(sp)[-1]

#Outliers
jarque.bera.test(sp[,1])

jarque.bera.test(ld_sp[,1])
boxplot(ld_sp)
boxplot.stats(ld_sp$Log_Differences)
outliers_date <- Boxplot(ld_sp)
ld_sp[outliers_date,] <- mean(ld_sp[,1])
plot(ld_sp[,1], type = 'l')
jarque.bera.test(ld_sp[,1])

#Stationary
plot(l_sp[,1], type = 'l')
acf(l_sp)

plot(ld_sp[,1], type = 'l')
acf(ld_sp)
kpss.test(ld_sp[,1])
adf.test(ld_sp[,1])

real_values_sp <- ld_sp[81:92, 1]
ld_sp <- ld_sp[2:80, 1]

###UNRATE###
un <- read.csv("UNRATE.csv")
un$DATE <- as.Date(un$DATE)
rownames(un) <- un[, 1]
un <- un[-1]

l_un <- log(un)
ld_un <- diff(l_un[,1])
ld_un <- data.frame(Log_Differences = c(ld_un))
rownames(ld_un) <- rownames(un)[-1]

dld_un <- diff(ld_un[,1])
dld_un <- data.frame(Diff_Log_Differences = c(dld_un))
rownames(dld_un) <- rownames(ld_un)[-1]

#Outliers
jarque.bera.test(un[,1])

jarque.bera.test(ld_un[,1])
boxplot(ld_un)
boxplot.stats(ld_un$Log_Differences)
outliers_date <- Boxplot(ld_un)
ld_un[outliers_date,] <- mean(ld_un[,1])
plot(ld_un[,1], type = 'l')
jarque.bera.test(ld_un[,1])

jarque.bera.test(dld_un[,1])
boxplot(dld_un)
boxplot.stats(dld_un$Diff_Log_Differences)
outliers_date <- Boxplot(dld_un)
dld_un[outliers_date,] <- mean(dld_un[,1])
plot(dld_un[,1], type = 'l')
jarque.bera.test(dld_un[,1])

#Stationary
plot(l_un[,1], type = 'l')
acf(l_un)

plot(ld_un[,1], type = 'l')
acf(ld_un)
kpss.test(ld_un[,1])
adf.test(ld_un[,1])

plot(dld_un[,1], type = 'l')
acf(dld_un)
kpss.test(dld_un[,1])
adf.test(dld_un[,1])

real_values_un <- dld_un[80:91, 1]
dld_un <- dld_un[1:79, 1]

###USS###
uss <- read.csv("USS.csv")
uss$DATE <- as.Date(uss$DATE)
rownames(uss) <- uss[, 1]
uss <- uss[-1]

l_uss <- log(uss)
ld_uss <- diff(l_uss[,1])
ld_uss <- data.frame(Log_Differences = c(ld_uss))
rownames(ld_uss) <- rownames(uss)[-1]

dld_uss <- diff(ld_uss[,1])
dld_uss <- data.frame(Diff_Log_Differences = c(dld_uss))
rownames(dld_uss) <- rownames(ld_uss)[-1]

#Outliers
jarque.bera.test(uss[,1])

jarque.bera.test(ld_uss[,1])
boxplot(ld_uss)
boxplot.stats(ld_uss$Log_Differences)
outliers_date <- Boxplot(ld_uss)
ld_uss[outliers_date,] <- mean(ld_uss[,1])
plot(ld_uss[,1], type = 'l')
jarque.bera.test(ld_uss[,1])

jarque.bera.test(dld_uss[,1])
boxplot(dld_uss)
boxplot.stats(dld_uss$Diff_Log_Differences)
outliers_date <- Boxplot(dld_uss)
dld_uss[outliers_date,] <- mean(dld_uss[,1])
plot(dld_uss[,1], type = 'l')
jarque.bera.test(dld_uss[,1])

#Stationary
plot(l_uss[,1], type = 'l')
acf(l_uss)

plot(ld_uss[,1], type = 'l')
acf(ld_uss)
kpss.test(ld_uss[,1])
adf.test(ld_uss[,1])

plot(dld_uss[,1], type = 'l')
acf(dld_uss[,1])
kpss.test(dld_uss[,1])
adf.test(dld_uss[,1])

a <- dld_uss
real_values_uss <- dld_uss[80:91, 1]
dld_uss <- dld_uss[1:79, 1]

################
###Stationarity
#See download data 

##Estimating the parameters
pacf(dld_gdp)
acf(dld_gdp)

auto.arima(dld_gdp)
modelGDP1 <- arima(dld_gdp, c(1,0,0))
modelGDP2 <- arima(dld_gdp, c(0,0,1))
modelGDP3 <- arima(dld_gdp, c(1,0,1))
modelGDP4 <- arima(dld_gdp, c(2,0,0))
modelGDP5 <- arima(dld_gdp, c(0,0,2))
modelGDP6 <- arima(dld_gdp, c(2,0,2))
modelGDP7 <- arima(dld_gdp, c(2,0,1))
modelGDP8 <- arima(dld_gdp, c(1,0,2))
modelGDP9 <- arima(dld_gdp, c(0,0,0))
modelGDP10 <- arima(dld_gdp, c(4,0,0))
modelGDP11 <- arima(dld_gdp, c(3,0,0))
AIC(modelGDP1, modelGDP2, modelGDP3, modelGDP4, modelGDP5, modelGDP6, modelGDP7, modelGDP8, modelGDP9, modelGDP10, modelGDP11)


#Estimating coefficients
coefficients(modelGDP2)
residusGDP <- residuals(modelGDP2)
plot(residusGDP, type='l', main = 'Residuals of the model ARIMA(0, 0, 1) of dld_gdp')

#Residuals = white noise ? 
jarque.bera.test(residusGDP)
Box.test(residusGDP, lag = 1, type = "Ljung-Box")
acf(residusGDP)
hist(residusGDP)
gqtest(residusGDP~1)

##Forecast
forecastGDP <- forecast(modelGDP2, h = 12)
plot(forecastGDP)
accuracy(forecastGDP, real_values_gdp)

gdp_for <- as.numeric(l_gdp[80]) + cumsum(ld_gdp[81]+cumsum(forecastGDP$mean))

plot(l_gdp, type = 'l', main = "Real vs. Forecasted log of GDP", xlab = 'Time')
lines(82:93, gdp_for, col = 'red')

accuracy(gdp_for, l_gdp[82:93])

print(forecastGDP$mean)
print(gdp_for)

gdp_dld <- dld_gdp
gdp_dld[80:91] <- real_values_gdp
plot(gdp_dld, type = 'l', main = 'Forecasted vs. Real dld_gdp', xlab = 'Time')
lines(80:91, forecastGDP$mean, col = "red")



################
data2 <- cbind(gdp, fgc, ir, mor, mrt, pop, un, uss, ipb, irl, sp, eu, uk, ppi)
corrplot(cor(data2))

data <- cbind(dld_gdp, dld_fgc, dld_ir, ld_mor, ld_mrt, dld_pop, dld_un, dld_uss, ld_ipb, ld_irl, ld_sp, ld_eu, ld_uk, ld_ppi)
corrplot(cor(data))

#Data without high correlation
data <- cbind(dld_gdp, dld_fgc, dld_ir, ld_mor, ld_mrt, dld_pop, dld_un, dld_uss, ld_ipb, ld_sp, ld_ppi, ld_eu)

VARselect(data)

grangertest(data[,2], data[,1], order=6)
grangertest(data[,3], data[,1], order=6)
grangertest(data[,4], data[,1], order=6)
grangertest(data[,5], data[,1], order=6)
grangertest(data[,6], data[,1], order=6)
grangertest(data[,7], data[,1], order=6)
grangertest(data[,8], data[,1], order=6)
grangertest(data[,9], data[,1], order=6)
grangertest(data[,10], data[,1], order=6)
grangertest(data[,11], data[,1], order=6)
grangertest(data[,12], data[,1], order=6)

dataVAR <- cbind(dld_gdp, ld_sp, dld_uss)

VARselect(dataVAR)
modelVAR <- VAR(dataVAR, p=2)
summary(modelVAR)

#Test on the residuals
residusVAR <- residuals(modelVAR)
Box.test(residusVAR[,1], type = "Ljung-Box")
Box.test(residusVAR[,2], type = "Ljung-Box")
Box.test(residusVAR[,3], type = "Ljung-Box")
JarqueBera.test(residusVAR[,1])
JarqueBera.test(residusVAR[,2])
JarqueBera.test(residusVAR[,3])
gqtest(residusVAR[,1] ~1)
gqtest(residusVAR[,2] ~1)
gqtest(residusVAR[,3] ~1)

forModelVAR <- predict(modelVAR, n.ahead = 12)
plot(forModelVAR, type = 'l')

accuracy(forModelVAR[['fcst']]$dld_gdp[,1], real_values_gdp)

gdp_forVAR <- as.numeric(l_gdp[80]) + cumsum(ld_gdp[81]+cumsum(forModelVAR[['fcst']]$dld_gdp[,1]))
plot(l_gdp, type = 'l', main = 'Real vs. Forecasted log GDP', xlab = 'Time')
lines(82:93, gdp_forVAR, col = 'red')

accuracy(gdp_forVAR, l_gdp[82:93])

plot(gdp_dld, type = 'l', main = 'Forecasted vs. Real dld_gdp', xlab = 'Time')
lines(80:91, forModelVAR[['fcst']]$dld_gdp[,1], col = "red")

print(forModelVAR)
print(gdp_forVAR)

###Modele VECM###
resultsJO <- ca.jo(dataVAR, K=2, type = 'trace', ecdet = "const")
summary(resultsJO)
vecm <- cajorls(resultsJO, r=2) 
print(vecm)

modelVECtoVAR <- vec2var(resultsJO, r=2)
summary(modelVECtoVAR)
forModelVECM <- predict(modelVECtoVAR, n.ahead = 12)
plot(forModelVECM)

#test of correlation on the residuals
Box.test(modelVECtoVAR$resid[,1], type = "Ljung-Box") 
Box.test(modelVECtoVAR$resid[,2], type = "Ljung-Box")
Box.test(modelVECtoVAR$resid[,3], type = "Ljung-Box")

JarqueBera.test(modelVECtoVAR$resid[,1])
JarqueBera.test(modelVECtoVAR$resid[,2])
JarqueBera.test(modelVECtoVAR$resid[,3])
gqtest(modelVECtoVAR$resid[,1] ~1)
gqtest(modelVECtoVAR$resid[,2] ~1)
gqtest(modelVECtoVAR$resid[,3] ~1)

##Evaluating : split of data : training and testing
accuracy(forModelVECM[["fcst"]]$dld_gdp[,1], real_values_gdp)
forModelVECM[["fcst"]]$dld_gdp[,1]
gdp_forVECM <- as.numeric(l_gdp[80]) + cumsum(ld_gdp[81]+cumsum(forModelVECM[['fcst']]$dld_gdp[,1]))
plot(l_gdp, type = 'l', main = 'Real vs. Forecasted log GDP', xlab = 'Time')
lines(82:93, gdp_forVECM, col = 'red')

accuracy(gdp_forVECM, l_gdp[82:93])

plot(gdp_dld, type = 'l', main = 'Forecasted vs. Real dld_gdp', xlab = 'Time')
lines(80:91, forModelVECM[['fcst']]$dld_gdp[,1], col = "red")

print(forModelVECM)
print(gdp_forVECM)


################
###############
