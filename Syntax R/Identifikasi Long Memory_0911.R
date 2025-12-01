data=read.csv(file.choose(),sep=";",dec=",",header=T)
str(data)

series=data[,1]
length(series)

library(forecast)
BoxCox.lambda(series)
plot(series,type='l')

# Fungsi menghitung autocovariance
autocovariance <- function(r, s){
  T <- length(r)
  r_bar <- mean(r)  # Calculate the mean
  
  sum_product <- sum((r[(s+1):T] - r_bar) * (r[1:(T-s)] - r_bar))
  
  gamma_s <- sum_product / (T)
  return(gamma_s)
}
autocovariance(series,0)
autocovariance(series,1)
autocovariance(series,2)

# Melakukan perhitungan autocovariance dan menyimpannya pada dataframe "result"
results = data.frame(Lag = integer(), Autocovariance = numeric()) # Membuat dataframe kosong
for (lag in c(0:(nrow(data)-1))){ # Menghitung autocovariance untuk t=0 hingga t=T-1
  autocov=autocovariance(series,lag)
  results=rbind(results, data.frame(Lag=lag, Autocovariance=autocov))
}
results

# Menyimpan hasil dari autocovariance menjadi file csv
#write.csv(results,file='hasil autocovariance.csv')

# Menghitung Statistik GPH
library(fracdiff)
fdGPH(series)
d = fdGPH(tfdata)$d
d

# Menghitung Statistik Hurst
library(pracma)
hurstexp(series)

# Menghitung Statistik GPH-TA
z=spec.taper(series)
fdGPH(z)

# Menghitung Statistik SPR
fdSperio(series)
?fdSperio

