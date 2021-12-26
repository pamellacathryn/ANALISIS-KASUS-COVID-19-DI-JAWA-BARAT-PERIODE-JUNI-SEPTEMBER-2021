#30 observasi
library(xlsx)
library(forecast)
library(tseries)
data = read.xlsx("data_tubes_andat.xlsx", sheetName="Sheet1")
data_1 = ts(data$`Suspected`)
data_2 = diff(data_1)
data_3 = diff(data_2)
data_4 = diff(data_3)

#Plot
plot(data_1,main="Grafik Suspected",ylab="Suspected",xlab="Tanggal",type='o')
plot(data_2,main="Grafik Diferensiasi 1x Suspected",ylab="Diferensiasi", xlab="Hari ke-",type='o')
plot(data_3,main="Grafik Diferensiasi 2x Suspected",ylab="Diferensiasi", xlab="Hari ke-",type='o')
plot(data_4,main="Grafik Diferensiasi 3x Suspected",ylab="Diferensiasi", xlab="Hari ke-",type='o')

#ACF
acf(data_1,main="Grafik ACF Suspected")
acf(data_2,main="Grafik ACF Suspected 1x Diferensiasi")
acf(data_3,main="Grafik ACF Suspected 2x Diferensiasi")
acf(data_4,main="Grafik ACF Suspected 3x Diferensiasi")

#ADF
adf.test(data_1)
adf.test(data_2)
adf.test(data_3)
adf.test(data_4)

#Metode Automatic
model_4 = auto.arima(data_4)
summary(model_4)

#Uji Diagnostik
checkresiduals(model_4)

#Prediksi
(prediksi = forecast(model_4, h = 5))
plot(prediksi,main="Grafik Suspected", ylab=  "Diferensiasi", xlab="Hari ke-",type='o')

