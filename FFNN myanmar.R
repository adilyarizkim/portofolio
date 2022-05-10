library(nnet)

#MEMBACA DATA#
myanmar=read.csv("D:/myanmarcovid.csv")
myanmar
View(myanmar)
y1t <- as.ts(myanmar$Total.case)

#READ THE DATA FROM TXT#
training=as.ts(y1t[1:237])                             #define training data
testing=as.ts(y1t[238:245])                             #define testing data
t=seq(1:245)
data.train=cbind(as.matrix(y1t),as.matrix(t))

#PREPROCESSING STANDARDIZED#
mean.Yt = mean(y1t)
sd.Yt   = sd(y1t)
Yt_std <- scale(data.train)
colnames(Yt_std)=c("Ytrain_std","t")
train=Yt_std[1:237,]
test=Yt_std[238:245,]
data=data.frame(train)
head(data)

#Model FFNN dengan 3 Neuron tanpa Skip Layer
set.seed(12)
best.model_NN=nnet(Ytrain_std~.,data=data,size=3,
                   linout=TRUE,skip = FALSE)
fits.model_NN=(best.model_NN$fitted.values)*sd.Yt+mean.Yt  #hasil ramalan data training

#Model FFNN dengan 3 Neuron dengan Skip Layer
set.seed(12)
best.model_NN=nnet(Ytrain_std~.,data=data,size=3,
                   linout=TRUE,skip = TRUE)
fits.model_NN=(best.model_NN$fitted.values)*sd.Yt+mean.Yt  #hasil ramalan data training

#ARSITEKTUR NEURAL NETWORK#
summary(best.model_NN)

n_fore=14
#FORECAST k-STEP AHEAD#
Ytest=c(train[,1],rep(0,n_fore))
for(i in (length(training)+1):(length(training)+n_fore))
{
  Xtest=(as.matrix(t(Yt_std[1,2])))
  Ytest[i]=predict(best.model_NN,Xtest)
}
fore.model_NN=Ytest[(length(training)+1):(length(training)+n_fore)]*sd.Yt+mean.Yt       #hasil ramalan data testing

#MENGHITUNG TINGKAT KESALAHAN PERAMALAN#
akurasi=matrix(0,1,6)
colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training",
                    "RMSE_testing","MAE_testing","MAPE_testing")
akurasi[,1]=accuracy(as.ts(fits.model_NN),training[13:304])[1,2]
akurasi[,2]=accuracy(as.ts(fits.model_NN),training[13:304])[1,3]
akurasi[,3]=accuracy(as.ts(fits.model_NN),training[13:304])[1,5]
akurasi[,4]=accuracy(fore.model_NN,testing)[1,2]
akurasi[,5]=accuracy(fore.model_NN,testing)[1,3]
akurasi[,6]=accuracy(fore.model_NN,testing)[1,5]
akurasi

#MEMBUAT PLOT PERBANDINGAN DATA AKTUAL DAN RAMALAN SEMUA NEURON#
a=min(min(fits.model_NN),min(training))   #batas bawah plot data training
b=max(max(fits.model_NN),max(training))   #batas atas plot data training
c=min(min(fore.model_NN),min(testing))    #batas bawah plot data testing
d=max(max(fore.model_NN),max(testing))    #batas atas plot data testing
#colors()                                  #warna yang tersedia di R

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))  #banyaknya gambar dan ukuran margin
par(mgp=c(1.3,0.5,0))                     #jarak judul label ke axis
warna=c("red2")

#PLOT DATA TRAINING#
plot(as.ts(training[13:304]),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*1.1,b*1.1))
box()
title("Data training",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1,seq(35,400,35)))
lines(as.ts(fits.model_NN),col=warna,lwd=2)

#PLOT DATA TESTING#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(c*0.9,d*1.2),cex.lab=0.8,axes=F)
box()
title("Data testing",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:7),labels=c(394:400))
lines(as.ts(fore.model_NN),col=warna,lwd=2)

#MEMBERI NAMA LEGEND#
legend("topleft",c("Data aktual","Ramalan"),
       col=c("black",warna),
       lwd=2,cex=0.7)

a=rbind(as.matrix(fits.model_NN),as.matrix(fore.model_NN))
write.csv(a,"NN.csv")

