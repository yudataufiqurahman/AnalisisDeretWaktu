data <- read.csv(file.choose(), header=T, sep=",", dec=".")
head(data)

Y = data$FOOD.PRICE.INDEX
X = data$WTISPLC

hargamakanan <- data.frame(X=X, Y=Y)
head(hargamakanan)


##STATISTIK DESKRIPTIF ----------

# SUMMARY
summary(hargamakanan)

# STANDAR DEVIASI
sd(hargamakanan$X)
sd(hargamakanan$Y)


##PARTISI DATA ----------
datatraining <- head(hargamakanan, round(0.81*nrow(data)))
datatesting <- tail(hargamakanan, round(0.19*nrow(data)))
nrow(datatraining)
nrow(datatesting)

##PLOT DATA ----------

# FOOD PRICE INDEX
Yt <- ts(datatraining$Y, start = c(2014,1), freq=12)
plot(Yt, ylab="Food Price Index", xlab="Tahun", main="Food Price Index (Training)")

# WTI SPOT PRICE LEVEL
Xt <- ts(datatraining$X, start = c(2014,1), freq=12)
plot(Xt, ylab="WTI Spot Price (US$)", xlab="Tahun", main="WTI Spot Price (Training)")


##PLOT ACF DAN PACF ----------
par(mfrow=c(1,2))

# FOOD PRICE INDEX
acf(Yt, lag.max=36)
pacf(Yt, lag.max=36)

# WTI SPOT PRICE
acf(Xt, lag.max=36)
pacf(Xt, lag.max=36)


##UJI STASIONER ----------
library(forecast)
library(tseries)

# FOOD PRICE INDEX
lambdaY <- BoxCox.lambda(Yt); lambdaY
adf.test(Yt)
diff_Yt <- diff(Yt, 1); adf.test(diff_Yt)

# WTI SPOT PRICE
lambdaX <- BoxCox.lambda(Xt); lambdaX
tf_Xt <- ((Xt^lambdaX)-1)/lambdaX
lambdaX_2 <- BoxCox.lambda(tf_Xt); lambdaX_2

adf.test(tf_Xt)
diff_tf_Xt <- diff(tf_Xt, 1); adf.test(diff_tf_Xt)

tf_Xt %>% tsdisplay(main="tf_Xt")
diff_tf_Xt %>% tsdisplay(main="diff_tf_Xt")


##PLOT ACF DAN PACF SETELAH PENANGANAN STASIONERITAS ----------
par(mfrow=c(1,2))

# FOOD PRICE INDEX
acf(diff_Yt, lag.max=36)
pacf(diff_Yt, lag.max=36)

# WTI SPOT PRICE
acf(diff_tf_Xt, lag.max=36)
pacf(diff_tf_Xt, lag.max=36)


##IDENTIFIKASI MODEL ARIMA UNTUK Xt ----------
library(lmtest)

modelI1 = Arima(tf_Xt, order=c(1,1,0), method="ML")
coeftest(modelI1)
modelI2 = Arima(tf_Xt, order=c(1,1,1), method="ML")
coeftest(modelI2)
modelI3 = Arima(tf_Xt, order=c(1,1,2), method="ML")
coeftest(modelI3)
modelI4 = Arima(tf_Xt, order=c(0,1,1), method="ML")
coeftest(modelI4)
modelI5 = Arima(tf_Xt, order=c(0,1,2), method="ML")
coeftest(modelI5)

# MODEL ARIMA YANG MEMENUHI ASUMSI UNTUK Xt
model_Xt <- data.frame(modelI1$aic, modelI2$aic, modelI3$aic, modelI4$aic, 
                       modelI5$aic); model_Xt
min(model_Xt)


##UJI ASUMSI RESIDUAL Xt ----------
rX = residuals(modelI1)

# UJI NORMALITAS Xt 
nX = length(rX)
meanX = mean(rX)
sdX = sd(rX)
resX = rnorm(nX,meanX,sdX)
ks.test(rX, resX)

# UJI AUTOKORELASI Xt
Box.test(rX, type="Ljung-Box")

# UJI HETEROSKEDASTISITAS Xt
Box.test(rX^2, type="Ljung-Box")


##PRE-WHITENING ----------
Box.test(residuals(modelI1), type=c("Ljung-Box"))
Box.test(residuals(modelI2), type=c("Ljung-Box"))
Box.test(residuals(modelI3), type=c("Ljung-Box"))
Box.test(residuals(modelI4), type=c("Ljung-Box"))


##KORELASI SILANG DERET OUTPUT DAN INPUT ----------
library(tfarima)

umXt <- um(tf_Xt, ar=0, i=1, ma=1)
umYt_Xt <- fit(umXt, diff_Yt)

par(mfrow=c(1,1))
pccf(tf_Xt, diff_Yt, um.x = umXt, um.y = umYt_Xt, lag.max=36)
pccf(residuals(umXt),residuals(umYt_Xt),lag.max=36)
#r=0 s=0 b=22


##IDENTIFIKASI FUNGSI TRANSFER AWAL ----------
library(MTS)
library(forecast)

pre_tf <- tfm1(Yt, tf_Xt, orderN=c(0,1,1), orderX=c(9,0,0))
pre_tf$residuals %>% ggtsdisplay()
pre_tf$residuals %>% checkresiduals()

acf(pre_tf$residuals)
pacf(pre_tf$residuals)
arima_Nt <- auto.arima(pre_tf$residuals, d=1, seasonal=FALSE)
arima_Nt
lmtest::coeftest(arima_Nt)

tf_impor <- tfest(Yt, tf_Xt, um.x = umXt, um.y = umYt_Xt)
tf_impor
noise_um <- um(Yt, ar=c(2), i=c(1), ma=c(0))
tfm_harga <- tfarima::tfm(Yt, inputs=list(tf_impor), noise=noise_um)
tfm_harga

##PERSAMAAN MODEL FUNGSI TRANSFER ----------
printLagpol(tfm_harga$inputs[[1]]$theta)
printLagpol(tfm_harga$inputs[[1]]$phi)
printLagpol(tfm_harga$noise$phi)


##UJI DIAGNOSTIK Y ----------
rY = residuals(tfm_harga)
Box.test(rY, type="Ljung-Box")
pccf(rY, tf_Xt, lag.max=36)


##HASIL FORECAST ----------
Ytest <- ts(datatesting$Y)
Xtest <- ts(datatesting$X)

umx_test <- um(Xtest, ar=0, i=1, ma=1)
umy_test <- fit(umx_test, Ytest)
tf_test <- tfest(Ytest, Xtest, um.x = umx_test, um.y = umy_test)
noise_um_test <- um(Ytest, ar=c(2), i=c(1), ma=c(0))
tfmy_test <- tfarima::tfm(Ytest, inputs=list(tf_test), noise=noise_um_test)
ramal_tfm <- Ytest - residuals(tfmy_test)


##PEMILIHAN MODEL TERBAIK (MAPE) ----------
plot(Ytest, type="l", col="black", xlab=" ", ylab=" "); par(new=TRUE)
plot(ramal_tfm, type="l", col="red", lty=2, main="Food Price Index (Testing)",
     ylab="Index", xaxt="n", yaxt="n")
legend("bottomright", 
       c("Data Aktual (Testing)", "Data Peramalan Fungsi Transfer"),
       col=c("black", "red"), lty=c(1,2), cex=0.7)

mape <- function(Y_aktual, Y_forecast){
  rumus = mean(abs(Y_aktual-Y_forecast)/Y_aktual)*100
  print(rumus)
}
mape(Ytest, ramal_tfm)


## PERIODE BERIKUTNYA ----------
outputY <- ts(hargamakanan$Y, start = c(2014,1), freq=12)
inputX <- ts(hargamakanan$X, start = c(2014,1), freq=12)

umx_data <- um(inputX, ar=0, i=1, ma=1)
umy_data <- fit(umx_data, outputY)
tf_data <- tfest(outputY, inputX, um.x = umx_data, um.y = umy_data)
noise_um_data <- um(outputY, ar=c(2), i=c(1), ma=c(0))
tfmy_data <- tfarima::tfm(outputY, inputs=list(tf_data), noise=noise_um_data)

HASIL <- predict.tfm(tfmy_data, n.ahead=12)
HASIL

plot(HASIL)
