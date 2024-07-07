# Set the working directory
setwd("C:/Users/user/Documents/PEMSPA/Pemspa/Pemspa") 

# load the spatial libraries
library("rgdal")
library("rgeos")
library("readxl")

# Load the data. You may need to alter the file directory
Data_Kabgarut <- read_excel("Data_Kabgarut.xlsx")
View(Data_Kabgarut)

# Load the output area shapefiles
polkecamatan <- readOGR(".", "BATAS_KECAMATAN")

# Join Data_Kabgarut to the shapefile
MergePD <- merge(polkecamatan, Data_Kabgarut, by.x="KECAMATAN", by.y="Kecamatan")

#Plot
plot(Data_Kabgarut$'Jumlah Covid',Data_Kabgarut$'Jumlah Pasar', xlab="Jumlah Covid", ylab="Jumlah Pasar", pch=19)+
  abline(lm(Data_Kabgarut$'Jumlah Pasar'~Data_Kabgarut$'Jumlah Covid'), col="blue", lwd = 4)
plot(Data_Kabgarut$'Jumlah Covid',Data_Kabgarut$'Jumlah Penduduk Lansia', xlab="Jumlah Covid", ylab="Jumlah Penduduk Lansia", pch=19)+
  abline(lm(Data_Kabgarut$'Jumlah Penduduk Lansia'~Data_Kabgarut$'Jumlah Covid'), col="blue", lwd = 4)
plot(Data_Kabgarut$'Jumlah Covid',Data_Kabgarut$'Total Jalan Beraspal (km)', xlab="Jumlah Covid", ylab="Total Jalan Beraspal (km)", pch=19)+
  abline(lm(Data_Kabgarut$'Total Jalan Beraspal (km)'~Data_Kabgarut$'Jumlah Covid'), col="blue", lwd = 4)
plot(Data_Kabgarut$'Jumlah Covid',Data_Kabgarut$'Jumlah Kendaraan Mobil', xlab="Jumlah Covid", ylab="Jumlah Kendaraan Mobil", pch=19)+
  abline(lm(Data_Kabgarut$'Jumlah Kendaraan Mobil'~Data_Kabgarut$'Jumlah Covid'), col="blue", lwd = 4)

#peta mode plot
library(tmap)
tmap_mode("plot")
tmap_options(max.categories = 62)+
  tm_shape(MergePD)+tm_fill("KECAMATAN")+
  tm_borders(alpha=.4)+
  tm_layout(title="Kabupaten Garut", title.size=5, legend.width=1, scale=0.4, legend.outside=TRUE, legend.text.size=2, legend.title.size=3)

#peta mode view
tmap_mode("view")
tmap_options(max.categories = 62)+
  tm_shape(MergePD)+tm_fill("KECAMATAN")+
  tm_borders(alpha=.4)+
  tm_layout(title="Kabupaten Garut", title.size=5, legend.width=1, scale=0.4, legend.outside=TRUE, legend.text.size=2, legend.title.size=3)

#Visualisasi Peta Covid menurut Jumlah Pasar (mode view)
tmap_mode("view")+
  tm_shape(MergePD)+
  tm_borders(alpha=.5)+ 
  tm_fill("Jumlah Covid", n=20, palette = "Blues")+
  tm_bubbles(scale=2, size = "Jumlah Pasar",n=10, col = "Jumlah Pasar", palette = "Reds", style = "kmeans", legend.size.show = FALSE, title.col = "Jumlah Pasar", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_layout(title="Kabupaten Garut", title.size=4, legend.width=1, scale=0.5, legend.outside=TRUE, legend.text.size=2, legend.title.size=3)

#Visualisasi Peta Covid menurut Jumlah Penduduk Lansia (mode view)
tmap_mode("view")+
  tm_shape(MergePD)+
  tm_borders(alpha=.5)+ 
  tm_fill("Jumlah Covid", n=20, palette = "Blues")+
  tm_bubbles(scale=2, size = "Jumlah Penduduk Lansia",n=10, col = "Jumlah Penduduk Lansia", palette = "Greys", style = "kmeans", legend.size.show = FALSE, title.col = "Jumlah Penduduk Lansia", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_layout(title="Kabupaten Garut", title.size=4, legend.width=1, scale=0.5, legend.outside=TRUE, legend.text.size=2, legend.title.size=3)

#Visualisasi Peta Covid menurut Total Jalan Beraspal (mode view)
tmap_mode("view")+
  tm_shape(MergePD)+
  tm_borders(alpha=.5)+ 
  tm_fill("Jumlah Covid", n=20, palette = "Blues")+
  tm_bubbles(scale=2, size = "Total Jalan Beraspal (km)",n=10, col = "Total Jalan Beraspal (km)", palette = "Greens", style = "kmeans", legend.size.show = FALSE, title.col = "Total Jalan Beraspal (km)", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_layout(title="Kabupaten Garut", title.size=4, legend.width=1, scale=0.5, legend.outside=TRUE, legend.text.size=2, legend.title.size=3)

#Visualisasi Peta Covid menurut Total Kendaraan Bermotor (mode view)
tmap_mode("view")+
  tm_shape(MergePD)+
  tm_borders(alpha=.5)+ 
  tm_fill("Jumlah Covid", n=20, palette = "Blues")+
  tm_bubbles(scale=2, size = "Jumlah Kendaraan Mobil",n=10, col = "Jumlah Kendaraan Mobil", palette = "YlGnBu", style = "kmeans", legend.size.show = FALSE, title.col = "Jumlah Kendaraan Mobil", border.col = "black", border.lwd = 0.1, border.alpha = 0.1)+
  tm_layout(title="Kabupaten Garut", title.size=4, legend.width=1, scale=0.5, legend.outside=TRUE, legend.text.size=2, legend.title.size=3)

#Metode pearson correlation
cordata <- Data_Kabgarut[,2:6]
cor(cordata)


#run spearman correlation test
cor.test(Data_Kabgarut$`Jumlah Covid`, Data_Kabgarut$`Jumlah Pasar`, method = "spearman", exact = FALSE)
cor.test(Data_Kabgarut$`Jumlah Covid`, Data_Kabgarut$`Jumlah Penduduk Lansia`, method = "spearman", exact = FALSE)
cor.test(Data_Kabgarut$`Jumlah Covid`, Data_Kabgarut$`Total Jalan Beraspal (km)`, method = "spearman", exact = FALSE)
cor.test(Data_Kabgarut$`Jumlah Covid`, Data_Kabgarut$`Jumlah Kendaraan Mobil`, method = "spearman", exact = FALSE)

#Regression Analysis Jumlah covid dengan jumlah pasar
regmodel_1a <- lm(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Pasar`)
plot(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Pasar`) + abline(regmodel_1a)
plotcase + abline(regmodel_1a, col = "red", lty = 1, lwd = 2)
#Regression Analysis Jumlah covid dengan jumlah penduduk lansia
regmodel_1b <- lm(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Penduduk Lansia`)
plot(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Penduduk Lansia`) + abline(regmodel_1b)
plotcase + abline(regmodel_1b, col = "red", lty = 1, lwd = 2)
#Regression Analysis Jumlah covid dengan total panjang jalan beraspal
regmodel_1c <- lm(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Total Jalan Beraspal (km)`)
plot(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Total Jalan Beraspal (km)`) + abline(regmodel_1c)
plotcase + abline(regmodel_1c, col = "red", lty = 1, lwd = 2)
#Regression Analysis Jumlah covid dengan jumlah kendaraan mobil
regmodel_1d <- lm(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Kendaraan Mobil`)
plot(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Kendaraan Mobil`) + abline(regmodel_1d)
plotcase + abline(regmodel_1d, col = "red", lty = 1, lwd = 2)
#Multiple regression with 2 independent variables
regmodel_2 <- lm(Data_Kabgarut$`Jumlah Covid`~ Data_Kabgarut$`Jumlah Pasar` + Data_Kabgarut$`Jumlah Penduduk Lansia` + Data_Kabgarut$`Total Jalan Beraspal (km)` + Data_Kabgarut$`Jumlah Kendaraan Mobil`)
summary(regmodel_2)
