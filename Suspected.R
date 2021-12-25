library(xlsx)
library(forecast)
library(tseries)
data = read.xlsx("data_tubes_andat.xlsx", sheetName="Sheet1")

#Data 1: Suspected
data_1 = ts(data$suspected)
plot(data_1, main="Grafik Suspected",
     ylab="Banyak Kasus",
     xlab="Hari ke-",
     type="o")
adf.test(data_1) #Uji Kestasioneran: Uji ADF

#karena belum stasioner, Diferensiasi ke-1
data_1_diff1 = diff(data_1)
plot(data_1_diff1, main="Grafik Diferensiasi 1x Suspected",
     ylab="Diferensiasi",
     xlab="Hari ke-",
     type="o")
adf.test(data_1_diff1) #Uji Kestasioneran: Uji ADF

#karena belum stasioner, Diferensiasi ke-2
data_1_diff2 = diff(data_1_diff1)
plot(data_1_diff2, main="Grafik Diferensiasi 2x Suspected",
     ylab="Diferensiasi",
     xlab="Hari ke-",
     type="o")
adf.test(data_1_diff2) #Uji Kestasioneran: Uji ADF

#sudah stasioner
#Identifikasi Orde: Manual
acf(data_1_diff2, main="Grafik ACF Suspected")
#Cut-off lag 0 -> q=0
pacf(data_1_diff2, main="Grafik PACF Suspected")
#Cut-off lag 4 -> p=4

#diperoleh ARIMA(p,d,q)=ARIMA(4,2,0)
model_ari1=arima(data_1, order=c(4,2,0))
summary(model_ari1)

#Uji Residual: Uji Ljung-Box
checkresiduals(model_ari1)
#karena p-value>alpha, maka model cocok dengan data

#Forecasting
(forecasting1=forecast(model_ari1,h=5))
plot(forecasting1,
     main="Grafik Prediksi Suspected",
     ylab="Banyak Kasus",
     xlab="Hari ke-",
     type="o")