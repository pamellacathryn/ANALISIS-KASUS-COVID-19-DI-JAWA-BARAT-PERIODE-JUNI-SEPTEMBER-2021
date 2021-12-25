library(xlsx)
library(forecast)
library(tseries)
data = read.xlsx("data_tubes_andat.xlsx", sheetName="Sheet1")

#Data 2: Active Cases
data_2 = ts(data$active_cases)
plot(data_2, main="Grafik Active Cases",
     ylab="Banyak Kasus",
     xlab="Hari ke-",
     type="o")
adf.test(data_2) #Uji Kestasioneran: Uji ADF

#karena belum stasioner, Diferensiasi ke-1
data_2_diff1 = diff(data_2)
plot(data_2_diff1, main="Grafik Diferensiasi 1x Active Cases",
     ylab="Diferensiasi",
     xlab="Hari ke-",
     type="o")
adf.test(data_2_diff1) #Uji Kestasioneran: Uji ADF

#karena belum stasioner, Diferensiasi ke-2
data_2_diff2 = diff(data_2_diff1)
plot(data_2_diff2, main="Grafik Diferensiasi 2x Active Cases",
     ylab="Diferensiasi",
     xlab="Hari ke-",
     type="o")
adf.test(data_2_diff2) #Uji Kestasioneran: Uji ADF

#sudah stasioner
#Identifikasi Orde: Manual
acf(data_2_diff2, main="Grafik ACF Active Cases")
#Cut-off lag 1 -> q=1
pacf(data_2_diff2, main="Grafik PACF Active Cases")
#Cut-off lag 2 -> p=2

#diperoleh ARIMA(p,d,q)=ARIMA(2,2,1)
model_ari2=arima(data_2, order=c(2,2,1))
summary(model_ari2)

#Uji Residual: Uji Ljung-Box
checkresiduals(model_ari2)
#karena p-value>alpha, maka model cocok dengan 

#Forecasting
(forecasting2=forecast(model_ari2,h=5))
plot(forecasting2,
     main="Grafik Prediksi Active Cases",
     ylab="Banyak Kasus",
     xlab="Hari ke-",
     type="o")