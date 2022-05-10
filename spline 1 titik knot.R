
GCV1=function(para)
{
  data=read.table("D:/tugas3.txt",header=TRUE)
  data=as.matrix(data)
  p=length(data[,1])
  q=length(data[1,])
  m=ncol(data)-para-1
  dataA=data[,(para+2):q]
  F=matrix(0,nrow=p,ncol=p)
  diag(F)=1 
  nk= length(seq(min(data[,2]),max(data[,2]),length.out=50))
  knot1=matrix(ncol=m,nrow=nk)
  for (i in (1:m))
  {
    for (j in (1:nk))
    {
      a=seq(min(dataA[,i]),max(dataA[,i]),length.out=50)
      knot1[j,i]=a[j]
    }
  }
  a1=length(knot1[,1])
  knot1=knot1[2:(a1-1),]
  aa=rep(1,p)
  data1=matrix(ncol=m,nrow=p)
  data2=data[,2:q]
  a2=nrow(knot1)
  GCV=rep(NA,a2)
  Rsq=rep(NA,a2)
  for (i in 1:a2)
  {
    for (j in 1:m)
    {
      for (k in 1:p)
      {
        if (data[k,(j+para+1)]<knot1[i,j]) data1[k,j]=0 else      data1[k,j]=data[k,(j+para+1)]-knot1[i,j]
      }
    }
    mx=cbind(aa,data2,data1)
    mx=as.matrix(mx)
    C=pinv(t(mx)%*%mx)
    B=C%%(t(mx)%%data[,1])
    yhat=mx%*%B
    SSE=0
    SSR=0
    for (r in (1:p))
    {
      sum=(data[r,1]-yhat[r,])^2
      sum1=(yhat[r,]-mean(data[,1]))^2
      SSE=SSE+sum
      SSR=SSR+sum1
    }
    Rsq[i]=(SSR/(SSE+SSR))*100
    MSE=SSE/p
    A=mx%%C%%t(mx)
    A1=(F-A) 
    A2=(sum(diag(A1))/p)^2
    GCV[i]=MSE/A2
  } 
  GCV=as.matrix(GCV)
  Rsq=as.matrix(Rsq)
  cat("======================================","\n")
  cat("Nilai Knot dengan Spline linear 1 knot","\n")
  cat("======================================","\n")
  print (knot1)
  cat("=======================================","\n")
  cat("Rsq dengan Spline linear 1 knot","\n")
  cat("=======================================","\n")
  print (Rsq)
  cat("=======================================","\n")
  cat("HASIL GCV dengan Spline linear 1 knot","\n")
  cat("=======================================","\n") 
  print (GCV)
  s1=min(GCV)
  print(max(Rsq))
  cat("======================================","\n")
  cat("HASIL GCV terkecil dengan Spline linear 1 knot","\n")
  cat("======================================","\n")
  cat(" GCV =",s1,"\n")
  write.table(GCV,file="D:/output GCV1.txt",sep=";")
  write.table(Rsq,file="D:/output Rsq1.txt",sep=";")
  write.table(knot1,file="D:/output knot1.txt",sep=";")
}
library(pracma)
GCV1(0)
