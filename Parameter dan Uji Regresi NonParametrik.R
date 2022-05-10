setwd("D:/Materi Semester 4/RegNonPar")
library(pracma)
library(MASS)
library(splines)
library(gtools)
library(Matrix)
library(readr)
knot1 = read.csv(file.choose())
para=0
library(readxl)
DATA = read.csv("D:/Materi Semester 4/RegNonPar/Program.csv"); DATA
DATA = as.matrix(DATA)
knot = as.matrix(knot1)
ybar = mean(DATA[,1])
m=para+2
p=nrow(DATA)
q=ncol(DATA)
dataA=cbind(DATA[,m],DATA[,m],DATA[,m], DATA[,m+1], DATA[,m+1], DATA[,m+1], DATA[,m+2], DATA[,m+2], DATA[,m+2])
dataA=as.matrix(dataA)
satu=rep(1,p)
n1=ncol(knot)-1
data.knot=matrix(ncol = n1, nrow = p)
for (i in 1:n1) {
  for (j in 1:p) {
    if(dataA[j,i]<knot[1,i]) data.knot[j,i]=0 else data.knot[j,i]=dataA[j,i]-knot[i]
    
  }
  
}
DATA
data.knot
mx = cbind(satu,DATA[,2], data.knot[,1], DATA[,3], data.knot[,2], DATA[,4], data.knot[,3], DATA[,5], data.knot[,4])
mx=as.matrix(mx)
print(mx)
B=(ginv(t(mx)%*%mx))%*%t(mx)%*%DATA[,1]
cat("============================","\n")
cat("Estimasi Parameter","\n")
B
cat("============================","\n")
n1=nrow(B) 
yhat=mx%*%B 
res=DATA[,1]-yhat 
SSE=sum((DATA[,1]-yhat)^2) 
SSR=sum((yhat-ybar)^2) 
SST=SSR+SSE 
MSE=SSE/(p-n1) 
MSR=SSR/(n1-1)
Rsq=(SSR/(SSR+SSE))*100 
#uji F (uji serentak) 
Fhit=MSR/MSE 
pvalue=pf(Fhit,(n1-1),(p-n1),lower.tail=FALSE) 
alpha=0.05
if (pvalue<=alpha) 
{ 
  cat("------------------------------------","\n") 
  cat("Kesimpulan hasil uji serentak","\n") 
  cat("------------------------------------","\n") 
  cat("Tolak Ho yakni minimal terdapat 1 prediktor yang signifikan","\n") 
  cat("","\n") 
} 
else 
{ 
  cat("------------------------------------","\n") 
  cat("Kesimpulan hasil uji serentak","\n") 
  cat("------------------------------------","\n") 
  cat("Gagal Tolak Ho yakni semua prediktor tidak berpengaruh 
signifikan","\n") 
  cat("","\n") 
} 
#uji t (uji individu) 
thit=rep(NA,n1) 
pval=rep(NA,n1) 
SE=sqrt(diag(MSE*(pinv(t(mx)%*%mx)))) 
cat("------------------------------------","\n") 
cat("Kesimpulan hasil uji individu","\n") 
cat("------------------------------------","\n") 
thit=rep(NA,n1) 
pval=rep(NA,n1) 
for (i in 1:n1) 
{ 
  thit[i]=B[i,1]/SE[i] 
  87 
  88 
  pval[i]=2*(pt(abs(thit[i]),(p-n1),lower.tail=FALSE)) 
  if (pval[i]<=alpha) cat("Tolak Ho yakni prediktor signifikan dengan 
pvalue",pval[i],"\n") else cat("Gagal tolak Ho yakni prediktor tidak 
signifikan dengan pvalue",pval[i],"\n") 
} 
thit=as.matrix(thit)
cat("=======================================","\n") 
cat("nilai t hitung","\n") 
cat("=======================================","\n") 
print (thit) 
cat("Analysis of Variance","\n") 
cat("======================================","\n") 
cat("Sumber df SS MS Fhit","\n") 
cat("Regresi ",(n1-1)," ",SSR," ",MSR,"",Fhit,"\n") 
cat("Error ",p-n1," ",SSE,"",MSE,"\n") 
cat("Total ",p-1," ",SST,"\n") 
cat("======================================","\n") 
cat("s=",sqrt(MSE)," Rsq=",Rsq,"\n") 
cat("pvalue(F)=",pvalue,"\n") 
write.csv(res,file="e:/output uji residual.txt") 
write.csv(pval,file="e:/output uji pvalue.txt") 
write.csv(mx,file="e:/output uji mx.txt") 
write.csv(yhat,file="e:/output uji yhat.txt") 
}
