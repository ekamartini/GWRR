
# # Menginstal Packager
# install.packages("spdep")
# install.packages("car")      
# install.packages("lmtest") 
# install.packages("spData")
# install.packages("readxl")
# install.packages("sf")
# install.packages("tmap")
# install.packages("RColorBrewer") 
# install.packages("spatialreg")
# install.packages("equatiomatic")
# install.packages("GWmodel")
# install.packages("openxlsx")
# install.packages("remotes")
# install.packages("spgwr")
# install.packages("tseries")
library(openxlsx)
library(tmap)
library(readxl)
library(car)
library(lmtest)
library(spdep)
library(spData)
library(sf)
library(RColorBrewer)
library(spatialreg)
library(dplyr)
library(equatiomatic)
library(GWmodel)
library(spgwr)
library(tseries)
library(gwrr)


#================================================= PENDAHULUAN =========================================================================
setwd("C:/Users/ekama/Documents/0. Olah Data") #mengeset working directory
getwd() #melihat working directory saat ini 
rm(list=ls(all=TRUE)) #menghapus objek yang tidak diperlukan 
ls() #cek objek yang ada

## Memasukkan data. Pilihan header = T diperlukan karena kita akan menggunakan header yang ada pada data
library(readxl)
dataskripsi <- read_excel("dataskripsi.fix.xlsx")

## Perintah head dapat digunakan untuk memberikan nama header data
head(dataskripsi)
summary (dataskripsi)
sd(dataskripsi$X1)

#===============================REGRESI LINEAR BERGANDA======================================================================
#Melakukan Regresi
regresi.ols <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7 , data = dataskripsi)
err.regresi<-residuals(regresi.ols) 
summary(regresi.ols) 

#-----------------------------UJI ASUMSI KLASIK-----------------------------------------------------------------------------
library(nortest)
lillie.test(err.regresi) #uji normalitas
library(lmtest)
dwtest(regresi.ols) #Uji Autokorelasi
library(lmtest) 
bptest(regresi.ols) #Uji Heteroskedastisitas
library(car) #Uji Multikolinearitas Global
vif(regresi.ols)

#Uji Multiko Gujarati
multiko.X1 <- lm(X1~X2+X5+X3+X4+X6+X7, data = dataskripsi)
summary(multiko.X1)
#=============================================================================GWR====================================
## Data Spasial
coordinates(dataskripsi) <- 3:4
class(dataskripsi)
head(dataskripsi)

U <- coordinates(dataskripsi)[,1]
V <- coordinates(dataskripsi)[,2]
nama_prov <- dataskripsi$Provinsi  
U
V
#------------------------------------------------------------ Bandwidth----------------------------------------------
## Adaptive Gaussian
bwd.GWR.gauss.ad <- bw.gwr(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                           data = dataskripsi, approach = "CV", 
                           kernel = "gaussian", adaptive = TRUE)
bwd.GWR.gauss.ad
hasil.GWR.gauss.ad <- gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                                data = dataskripsi, bw = bwd.GWR.gauss.ad, 
                                kernel = "gaussian", adaptive = TRUE)
## Adaptive Bisquare 
bwd.GWR.bisquare.ad <- bw.gwr(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                              data = dataskripsi, approach = "CV", 
                              kernel = "bisquare", adaptive = TRUE)
bwd.GWR.bisquare.ad

hasil.GWR.bisquare.ad <- gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                                   data = dataskripsi, bw = bwd.GWR.bisquare.ad, 
                                   kernel = "bisquare", adaptive = TRUE)
## Adaptive Exponential
bwd.GWR.exp.ad <- bw.gwr(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                         data = dataskripsi, approach = "CV", 
                         kernel = "exponential", adaptive = TRUE)
bwd.GWR.exp.ad
hasil.GWR.exp.ad <- gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                              data = dataskripsi, bw = bwd.GWR.exp.ad, 
                              kernel = "exponential", adaptive = TRUE)
## Fixed Bisquare
bwd.GWR.bisquare.fix <- bw.gwr(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                               data = dataskripsi, approach = "CV", 
                               kernel = "bisquare", adaptive = FALSE)
bwd.GWR.bisquare.fix
hasil.GWR.bisquare.fix <- gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                                    data = dataskripsi, bw = bwd.GWR.bisquare.fix, 
                                    kernel = "bisquare", adaptive = FALSE)
#Fixed Expponential
bwd.GWR.exp.fix <- bw.gwr(Y ~ X1+X2+X3+X4+X5+X6+X7,
                          data = dataskripsi,
                          approach = "CV",
                          kernel = "exponential",
                          adaptive = FALSE)
hasil.GWR.exp.fix <- gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7,
                               data = dataskripsi,
                               bw = bwd.GWR.exp.fix,
                               kernel = "exponential",
                               adaptive = FALSE)
bwd.GWR.exp.fix

## Fixed Gaussian
bwd.GWR.gauss.fix <- bw.gwr(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                            data = dataskripsi, approach = "CV", 
                            kernel = "gaussian", adaptive = FALSE)
bwd.GWR.gauss.fix
hasil.GWR.gauss.fix <- gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7, 
                                 data = dataskripsi, bw = bwd.GWR.gauss.fix, 
                                 kernel = "gaussian", adaptive = FALSE)
## Bandwidth per Lokasi (Adaptive) 
bwd.lokasiA1 <- gw.adapt(dp = cbind(U, V), fp = cbind(U, V), 
                         quant = hasil.GWR.gauss.ad$GW.arguments$bw/ nrow(dataskripsi))
write.xlsx(as.data.frame(bwd.lokasiA1), "Bandwidth_Adaptive_Gaussian.xlsx")

bwd.lokasiA2 <- gw.adapt(dp = cbind(U, V), fp = cbind(U, V), 
                         quant = hasil.GWR.bisquare.ad$GW.arguments$bw/ nrow(dataskripsi))
write.xlsx(as.data.frame(bwd.lokasiA2), "Bandwidth_Adaptive_Bisquare.xlsx")
bwd.lokasiA3 <- gw.adapt(dp = cbind(U, V), fp = cbind(U, V), 
                         quant = hasil.GWR.exp.ad$GW.arguments$bw/nrow(dataskripsi))
write.xlsx(as.data.frame(bwd.lokasiA3), "Bandwidth_Adaptive_Exponential.xlsx")


## Matriks Jarak Euclidean
jar <- gw.dist(dp = coordinates(dataskripsi))
n <- nrow(dataskripsi)
jar
#======================Matriks Pembobot===================================
## Pembobot Adaptive Gaussian
bdwt <- as.matrix(bwd.lokasiA1)
pembobot.ad.gauss <- matrix(0, n, n)
for(i in 1:n)for(j in 1:n){
  pembobot.ad.gauss[i,j] <- exp((-1/2) * (jar[i,j]/bdwt[i,])^2)
}
rownames(pembobot.ad.gauss) <- nama_prov
colnames(pembobot.ad.gauss) <- nama_prov
write.xlsx(as.data.frame(pembobot.ad.gauss), "Pembobot_Adaptive_Gaussian.xlsx", rowNames = TRUE)

## Pembobot Adaptive Bisquare
bdwt <- as.matrix(bwd.lokasiA2)
pembobot.ad.bis <- matrix(0, n, n)
for(i in 1:n)for(j in 1:n){
  pembobot.ad.bis[i,j] <- (1 - (jar[i,j]/bdwt[i,])^2)^2
}
rownames(pembobot.ad.bis) <- nama_prov
colnames(pembobot.ad.bis) <- nama_prov
write.xlsx(as.data.frame(pembobot.ad.bis), "Pembobot_Adaptive_Bisquare.xlsx", rowNames = TRUE)

## Pembobot Adaptive Exponential
bdwt <- as.numeric(bwd.lokasiA3)
pembobot.ad.exp <- matrix(0, n, n)

for(i in 1:n){
  for(j in 1:n){
    pembobot.ad.exp[i,j] <- exp(-jar[i,j] / bdwt[i])
  }
}
rownames(pembobot.ad.exp) <- nama_prov
colnames(pembobot.ad.exp) <- nama_prov
write.xlsx(as.data.frame(pembobot.ad.exp),"Pembobot_Adaptive_Exponential.xlsx",rowNames = TRUE)


## Pembobot Fixed Gaussian
bw.fixed.gauss <- hasil.GWR.gauss.fix$GW.arguments$bw
pembobot.fix.gauss <- matrix(0, n, n)
for(i in 1:n)for(j in 1:n){
  pembobot.fix.gauss[i,j] <- exp((-1/2) * (jar[i,j]/bw.fixed.gauss)^2)
}
rownames(pembobot.fix.gauss) <- nama_prov
colnames(pembobot.fix.gauss) <- nama_prov
write.xlsx(as.data.frame(pembobot.fix.gauss), "Pembobot_Fixed_Gaussian.xlsx", rowNames = TRUE)

#Fixed Exponential kernel
bw.fixed.exp <- hasil.GWR.exp.fix$GW.arguments$bw
pembobot.fix.exp <- matrix(0, n, n)
for(i in 1:n) for(j in 1:n){
  pembobot.fix.exp[i,j] <- exp( -0.5 * ( jar[i,j] / bw.fixed.exp )^2 )
}
rownames(pembobot.fix.exp) <- nama_prov
colnames(pembobot.fix.exp) <- nama_prov
write.xlsx(as.data.frame(pembobot.fix.exp),
           "Pembobot_Fixed_Exponential.xlsx",
           rowNames = TRUE)

## Pembobot Fixed Bisquare 
bw.fixed.bisq <- hasil.GWR.bisquare.fix$GW.arguments$bw
pembobot.fix.bisq <- matrix(0, n, n)
for(i in 1:n)for(j in 1:n){
  d <- jar[i,j]
  if(d <= bw.fixed.bisq){
    pembobot.fix.bisq[i,j] <- (1 - (d/bw.fixed.bisq)^2)^2
  } else {
    pembobot.fix.bisq[i,j] <- 0
  }
}
rownames(pembobot.fix.bisq) <- nama_prov
colnames(pembobot.fix.bisq) <- nama_prov
write.xlsx(as.data.frame(pembobot.fix.bisq), "Pembobot_Fixed_Bisquare.xlsx", rowNames = TRUE)

##Perbandingan
AIC <- c(
  hasil.GWR.bisquare.ad$GW.diagnostic$AIC,
  hasil.GWR.gauss.ad$GW.diagnostic$AIC,
  hasil.GWR.exp.ad$GW.diagnostic$AIC,
  hasil.GWR.bisquare.fix$GW.diagnostic$AIC,
  hasil.GWR.exp.fix$GW.diagnostic$AIC,       # exponential FIx ke–4
  hasil.GWR.gauss.fix$GW.diagnostic$AIC      # gaussian FIx ke–5
)

R2 <- c(
  hasil.GWR.bisquare.ad$GW.diagnostic$gw.R2,
  hasil.GWR.gauss.ad$GW.diagnostic$gw.R2,
  hasil.GWR.exp.ad$GW.diagnostic$gw.R2,
  hasil.GWR.bisquare.fix$GW.diagnostic$gw.R2,
  hasil.GWR.exp.fix$GW.diagnostic$gw.R2,     # exponential FIx ke–4
  hasil.GWR.gauss.fix$GW.diagnostic$gw.R2    # gaussian FIx ke–5
)

R2adj <- c(
  hasil.GWR.bisquare.ad$GW.diagnostic$gwR2.adj,
  hasil.GWR.gauss.ad$GW.diagnostic$gwR2.adj,
  hasil.GWR.exp.ad$GW.diagnostic$gwR2.adj,
  hasil.GWR.bisquare.fix$GW.diagnostic$gwR2.adj,
  hasil.GWR.exp.fix$GW.diagnostic$gwR2.adj,  # exponential FIx ke–4
  hasil.GWR.gauss.fix$GW.diagnostic$gwR2.adj # gaussian FIx ke–5
)

RSS <- c(
  hasil.GWR.bisquare.ad$GW.diagnostic$RSS.gw,
  hasil.GWR.gauss.ad$GW.diagnostic$RSS.gw,
  hasil.GWR.exp.ad$GW.diagnostic$RSS.gw,
  hasil.GWR.bisquare.fix$GW.diagnostic$RSS.gw,
  hasil.GWR.exp.fix$GW.diagnostic$RSS.gw,     # exponential FIx ke–4
  hasil.GWR.gauss.fix$GW.diagnostic$RSS.gw    # gaussian FIx ke–5
)
#Terbaik
tabel <- cbind(RSS,R2,R2adj,AIC)
rownames(tabel) <- c("Adaptive Bisquare", "Adaptive Gaussian", "Adaptive Exponential", "Fixed Bisquare","Fixed Exponential", "Fixed Gaussian")
data.frame(tabel)
hasil.GWR.exp.fix

#=====================================================MODEL GWR================================
GWRModel.exp.fix<-gwr.basic(Y ~ X1+X2+X3+X4+X5+X6+X7, data = dataskripsi, 
                            bw=bw.fixed.exp, kernel = "exponential", adaptive = FALSE, p=2) 

GWRModel.exp.fix
names(GWRModel.exp.fix) 
names(GWRModel.exp.fix$SDF) #Objek dalam spasial data frame
names(GWRModel.exp.fix$results)
parameter.GWR <- as.data.frame(GWRModel.exp.fix$SDF)
parameter.GWR 
#---------------------------------Menyimpan Hasil GWR dalam Excel-----------------------------------------
library(openxlsx)
# Ubah SDF menjadi data frame
parameter.GWR <- as.data.frame(GWRModel.exp.fix$SDF)
# Ambil hanya kolom beta (tanpa SE, TV, dll)
beta_cols <- c("Intercept",
               "X1",
               "X2",
               "X3",
               "X4",
               "X5",
               "X6",
               "X7")
# Gabungkan ID + yhat + beta
df_gwr <- data.frame(
  id = 1:nrow(parameter.GWR),
  yhat = parameter.GWR$yhat,
  parameter.GWR[, beta_cols]
)
# Export ke Excel
write.xlsx(
  df_gwr,
  "hasil_gwr.xlsx",
  overwrite = TRUE
)

#=====================================================MULTIKOLINEARITAS LOKAL=================================
#------------------------------------VIF LOKAL-------------------------------------
VIFLokal.exp <- gwr.collin.diagno(
  Y ~ X1+X2+X3+X4+X5+X6+X7,
  data = dataskripsi,
  bw = bw.fixed.exp,
  kernel = "exponential",
  adaptive = FALSE,
  p = 2
)
VIFLokal.exp$VIF
library(openxlsx)

#-------------Menyimpan ke Excel---------------------------------------
# Ubah hasil menjadi data frame
vif_local <- as.data.frame(VIFLokal.exp$SDF)
# Ambil hanya kolom VIF (biasanya bernama X1_VIF, dst)
vif_cols <- grep("VIF", names(vif_local), value = TRUE)
# Buat data final
df_vif <- data.frame(
  id = 1:nrow(vif_local),
  vif_local[, vif_cols]
)
# Export ke Excel
write.xlsx(
  df_vif,
  "VIF_Lokal_GWR.xlsx",
  overwrite = TRUE
)
#----------------------------------------CONDITION NUMBER-----------------------
#MASUKKAN KOORDINAT
locs <- cbind(dataskripsi$U, dataskripsi$V)
class(locs)
head(locs)
data.skripsi.frame <- as.data.frame(dataskripsi)
multiko.lokal <- gwr.vdp( Y ~ X1+X2+X3+X4+X5+X6+X7, data = data.skripsi.frame,locs = locs, phi= bw.fixed.exp)
multiko.lokal

#MENYIMPANKE EXCEL
library(openxlsx)
# Ubah hasil menjadi data frame
vdp_local <- as.data.frame(multiko.lokal)
# Tambahkan ID lokasi
vdp_local$id <- 1:nrow(vdp_local)
# Pindahkan ID ke kolom pertama
vdp_local <- vdp_local[, c("id", setdiff(names(vdp_local), "id"))]
# Export ke Excel
write.xlsx(
  vdp_local,
  "VDP_Lokal_GWRR.xlsx",
  overwrite = TRUE
)
#=============================================================GWRR=============================================

GWRRModel.exp.fix <- gwrr.est(Y ~ X1+X2+X3+X4+X5+X6+X7, locs=locs, data=data.skripsi.frame,
                              kernel="exp", bw=TRUE, rd=TRUE)
GWRRModel.exp.fix


## ----------------------------------------------Menyimpan hasil GWRR dalam Excel"-------------------------------
# yhat
df_yhat <- data.frame(
  id = 1:length(GWRRModel.exp.fix$yhat),
  yhat = GWRRModel.exp.fix$yhat
)
# beta (transpose supaya baris = lokasi)
df_beta <- as.data.frame(t(GWRRModel.exp.fix$beta))
df_beta$id <- 1:nrow(df_beta)
# pindahkan id ke kolom pertama
df_beta <- df_beta[, c("id", setdiff(names(df_beta), "id"))]
library(openxlsx)
write.xlsx(
  list(
    yhat = df_yhat,
    beta = df_beta
  ),
  file = "hasil_gwrr_percobaan4.xlsx",
  overwrite = TRUE
)
df_gwrr <- cbind(
  id = 1:length(GWRRModel.exp.fix$yhat),
  yhat = GWRRModel.exp.fix$yhat,
  as.data.frame(t(GWRRModel.exp.fix$beta))
)
library(openxlsx)
write.xlsx(df_gwrr, "hasil_gwrr_satu_sheet.xlsx", overwrite = TRUE)


#=================================================KEEFEKTIFAN MODEL DENGAN RANGE========================
#Range GWRR
beta_gwrr <- GWRRModel.exp.fix$beta   # matriks 8 x 38
rownames(beta_gwrr) <- c("Intercept",
                         "X1",
                         "X2",
                         "X3",
                         "X4",
                         "X5",
                         "X6",
                         "X7")
range_gwrr <- t(apply(beta_gwrr, 1, function(x){
  c(Min=min(x),
    Max=max(x),
    Range=max(x)-min(x),
    Mean = mean(x))
}))
range_gwrr

#Range GWR

beta_gwr <- GWRModel.exp.fix$SDF@data
range_gwr <- t(apply(beta_gwr[, c("Intercept",
                                  "X1",
                                  "X2",
                                  "X3", 
                                  "X4",
                                  "X5",
                                  "X6",
                                  "X7")], 
                     2, function(x){
                       c(Min = min(x),
                         Max = max(x),
                         Range = max(x) - min(x),
                         Mean = mean(x))
                     }))

range_gwr

