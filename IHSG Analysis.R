#Indonesia IHSG Data
library(readxl)
library(tseries)
library(lmtest)
library(forecast)
library(TSA)
library(FinTS)

# Input Data
data <- read_xlsx("C:/Users/ASUS/Documents/DataIHSGCovid.xlsx", sheet="IHSG")
data
indeks <- data$Indeks

# Descriptive Statistic Data
summary(indeks)

# Time Series Data
library(tseries)
indeksts<- ts(indeks)

# Scatter Plot Time Series Data
plot(indeksts, main = "Daily IHSG Data", ylab = "IHSG", xlab = "day", type = 'o')
abline(h = mean(indeks), col = 'blue')

# Stationarity Test
# Plot ACF
acf(indeksts, main = 'IHSG ACF plot', lag.max = 170)
pacf(indeksts, main = 'IHSG PACF plot', lag.max = 170)
# ADF(Augmented Dickey-Fuller) Test
adf.test(indeksts)

# Non Stasionary Differentiation
indekstsdiff = diff(indeksts) 
plot(indekstsdiff, main = "1 Time Differentiation IHSG Graph", ylab = "Differentiation", xlab = "Week", type = 'o') 
abline(h = mean(indekstsdiff), col = 'blue')

# Plot ACF 1x Diferensiasi
acf(indekstsdiff, main = '1 Time Differentiation IHSG ACF Graph', lag.max = 170)
pacf(indekstsdiff, main = '1 Time Differentiation IHSG PACF Graph', lag.max = 170)
# ADF(Augmented Dickey-Fuller) Test
adf.test(indekstsdiff)

# Manual Model Estimation
# ACF Cut Off lag 3
# PACF Cut off lag 2
model_arima1 = arima(indeksts,order = c(2,1,0))
summary(model_arima1)
model_arima2 = arima(indeksts,order = c(0,1,2))
summary(model_arima2)
model_arima3 = arima(indeksts,order = c(2,1,2))
summary(model_arima3)

# Automatic Parameter Estimation
model = auto.arima(indeksts,max.p=10,max.q=10,
                   seasonal=FALSE, stationary = TRUE)
summary(model)

# Data Coeficient Significancy Test 
coeftest(model_arima1)

# Diagnostic Test
qqnorm(residuals(model_arima1))
qqline(residuals(model_arima1))
hist(residuals(model_arima1))
ks.test(residuals(model_arima1),"pnorm")
Box.test(residuals(model_arima1),lag = 170, type="Ljung-Box")
checkresiduals(model_arima1)

# Heteroscedastic Test
resid<-residuals(model_arima1)
ArchTest(resid)
# none

# Data Check
data_pred <- indeksts - residuals(model_arima1)
ts.plot(data_pred,indeksts, xlab = 'Hari', ylab = 'IHSG', 
        col = c('red', 'blue'), main = 'Perbandingan Data IHSG Asli dengan Model')
legend("topleft",legend=c("Data IHSG","Model ARIMA (2,1,0)"),cex=0.75,lty=1,
       col=c("dark red","blue"),pch=c(19,NA))

# Forecasting
(prediksi = forecast(model_arima1, h = 10))
autoplot(prediksi, main="Forecasting IHSG",ylab="IHSG",xlab="Hari")
points(indeksts, cex = .25, col = "dark red", pch=19)
lines(indeksts, col = "red")
abline(v=1998 ,lty = 2)
legend("topleft",legend=c("IHSG","Forecasting"),cex=0.75,lty=1,
       col=c("dark red","blue"),pch=c(19,NA))
