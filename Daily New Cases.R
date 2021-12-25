library(xlsx)
library(forecast)
library(tseries)
data = read.xlsx("data_tubes_andat.xlsx", sheetName="Sheet1")

#Data 3: Daily New Cases
data_3 = ts(data$daily_new_cases)
plot(data_3, main="Daily New Cases",
     ylab="Banyak Kasus",
     xlab="Hari ke-",
     type="o")
adf.test(data_3) #Uji Kestasioneran: Uji ADF

#karena belum stasioner, Diferensiasi ke-1
data_3_diff1 = diff(data_3)
plot(data_3_diff1, main="Grafik Diferensiasi 1x Daily New Cases",
     ylab="Diferensiasi",
     xlab="Hari ke-",
     type="o")
adf.test(data_3_diff1) #Uji Kestasioneran: Uji ADF

#sudah stasioner
#Identifikasi Orde: Manual
acf(data_3_diff1, main="Grafik ACF Daily New Cases")
#Cut-off lag 0 -> q=0
pacf(data_3_diff1, main="Grafik PACF Daily New Cases")
#Cut-off lag 4 -> p=4

#diperoleh ARIMA(p,d,q)=ARIMA(4,1,0)
model_ari3=arima(data_3, order=c(4,1,0))
summary(model_ari3)

#Uji Residual: Uji Ljung-Box
checkresiduals(model_ari3)
#karena p-value>alpha, maka model cocok dengan data

#Forecasting
(forecasting3=forecast(model_ari3,h=5))
plot(forecasting3,
     main="Grafik Prediksi Daily New Cases",
     ylab="Banyak Kasus",
     xlab="Hari ke-",
     type="o")