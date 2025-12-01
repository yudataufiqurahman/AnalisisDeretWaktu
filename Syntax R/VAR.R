# VAR

# library
library(dplyr)
library(padr)
library(lubridate)
library(forecast)
library(ggplot2)
library(MLmetrics)
library(lmtest)
library(vars)

# input data
data <-read.csv("C:\\Users\\ASUS\\OneDrive\\Documents\\Perkuliahan\\Semester 5\\ADDW 2\\VAR VECM\\weather.csv",header=T,sep=",",dec=".")
head(data)

# Step 1
data <- data[data$country == "Finland",]


# cek missing value
data %>% is.na() %>% colSums()
# mengatasi missing value (imputasi dengan metode forward filling)
# data <- data %>%
  #pad(start_val = min(data$Date), end_val = max(data$Date)) %>% 
  #mutate_if(is.numeric, na.locf)
# cek lagi untuk memastikan masih ada tidak missing valuenya
# colSums(is.na(data))

## ---- Exploratory Data Analysis ----
# visualisasi data time series
ts(data[,c(3:7)], start = c(2015,1), freq = 365) %>% 
  autoplot(facets = TRUE)
nrow(data)

snow_depth <- ts(data$snow_depth, start = c(2015, 1), freq = 365) 
snow_depth
tavg <- ts(data$tavg, start = c(2015, 1), freq = 365)

#Pengujian Stasioneritas Varians
BoxCox.lambda(snow_depth)
BoxCox.lambda(tavg)
#Lambda = 1 sudah stasioner

#Pengujian Stasioneritas
library(tseries)
adf.test(snow_depth)
adf.test(tavg)
#H0 : Data Tidak Stasioner
#H1 : Data Stasioner
#alpha 0.05
#Kriteria Uji : Tolak H0 jika pvalue < alpha
## Differencing 1 kali
df.snow=diff(snow_depth, 1)
df.tavg=diff(tavg, 1)
# Cek Stasioner Rata-Rata
adf.test(df.snow) #OK
adf.test(df.tavg) #OK
#Plot ACF
Acf(df.snow, main = "Plot ACF Snow Depth", lag.max = 50)
Acf(df.tavg, main = "Plot ACF Temperature Average", lag.max = 50)
#Plot PACF
Pacf(df.snow, main = "Plot PACF Snow Depth", lag.max = 50)
Pacf(df.tavg, main = "Plot PACF Temperature Average", lag.max = 50)

# Granger Causality Test (mengevaluasi hubungan variabel, dilakukan dua arah)
# H0: X tidak dapat digunakan untuk memprediksi Y
# H1: X dapat digunakan untuk memprediksi Y
grangertest(x = df.snow, y = df.tavg)
# tolak H0, jadi variabel snow depth di masa lampau dapat digunakan untuk memprediksi nilai temperature average di masa depan
grangertest(x = df.tavg, y = df.snow)
# tolak H0, jadi variabel temperature average di masa lampau dapat digunakan untuk memprediksi nilai snow depth di masa depan

## ---- Modelling ----
# buat kolom snow_depth dan tavg menjadi object time series
data_ts <- ts(data[,c("snow_depth", "tavg")], start = c(2015,1,1), end = c(2019,31,12), frequency = 365)
data_ts %>% autoplot(facets = TRUE)
# splitting data train dan data test
n_train <- as.integer(0.8*nrow(data_ts))
train_data <- head(data_ts, n_train)
nrow(train_data)
test_data <- tail(data_ts, -n_train)
nrow(test_data)
# penentuan nilai lag (Ordo VAR)
VARselect(train_data, lag.max = 10, season = 365)
# pilih dari nilai AIC, HQ, SC, atau FPE

# membuat model dengan lag yang terpilih
model <- VAR(train_data, p=7, season = 365)

## ---- Evaluation ----
prediction <- forecast(model, h = nrow(test_data))
prediction
# Visualisasi nilai forecast dengan data testing
# plot snow depth
train_data[,"snow_depth"] %>% 
  autoplot(main = "SNOW DEPTH") +
  autolayer(test_data[,"snow_depth"], series = "Test Data") +
  autolayer(prediction$forecast$snow_depth$mean, series = "Forecast")
# Plot tavg
train_data[,"tavg"] %>% autoplot(main = "TEMPERATURE AVERAGE") +
  autolayer(test_data[,"tavg"], series = "Test Data") +
  autolayer(prediction$forecast$tavg$mean, series = "Forecast")

# perhatikan nilai MAE (mengetahui seberapa jauh rata-rata penyimpangan prediksi kita)
mae_snow <- MAE(prediction$forecast$snow_depth$mean, test_data[,"snow_depth"])
mae_tavg <- MAE(prediction$forecast$tavg$mean, test_data[,"tavg"])
paste("MAE untuk Snow Depth: ", mae_snow)
paste("MAE untuk Temperature Average: ", mae_tavg)

 #Diagnostik Model
#=====Uji Normalitas Data======
#H0: residual berdistribusi normal
#H1: residual tidak berdistribusi normal
r1 <- residuals(model)
n1=length(r1)
mean1=mean(r1)
sd1=sd(r1)
res1=rnorm(n1,mean1,sd1)
hasil1=ks.test(r1,res1)
hasil1

#=====Uji White Noise-Autokorelasi=====
#H0: tidak ada autokorelasi
#H1: ada autokorelasi
autokol <- serial.test(model,lags.pt =7,type = "PT.asymptotic")
autokol

#=====Uji White Noise-Heteroskedastisitas=====
#H0: homoskedatis
#H1: heteroskedastis
uh <- arch.test(model, lags.multi = 7, multivariate.only = TRUE)
uh

# testing for structural stability
cusum <- stability(model, type = "OLS-CUSUM")

windows()
plot(cusum) #grafik berada dalam selang kepercayaan sehingga model yang dimiliki cukup stabil untuk digunakan

## Generate irfs ====
irf.lb <- irf(model, impulse = "snow_depth", response = "tavg",n.ahead = 40, boot = TRUE)
plot(irf.lb) # reponse of gdp to unemploy shocks

## ---- Forecasting ----
# pemodelan
final_model <- VAR(data_ts, p = 7, season = 365)
# forecasting
forecasting <- forecast(final_model, h = 365)
predictions <- predict(final_model, n.ahead = 365, ci = 0.95)
# visualisasi hasil forecasting
plot(predictions, names = "snow_depth")
plot(predictions, names = "tavg")
