library(class) ## a library with lots of classification tools
library(kknn) ## knn library



rm(list=ls())
df_usa <- read.csv(file = 'C:/Users/apurv/OneDrive/Desktop/StartupSuccess.csv')
attach(df_usa)
n = dim(df_usa)[1]
n
plot(df_usa$funding_rounds,df_usa$status_label)
plot(df_usa$total_investment,df_usa$status_label)
plot(df_usa$venture,df_usa$status_label)
plot(df_usa$seed,df_usa$status_label)
plot(df_usa$diff_funding_year,df_usa$status_label)

train = data.frame(funding_rounds,status_label)
test = data.frame(funding_rounds,status_label)
ind = order(test[,1])
test =test[ind,]

MSE = NULL

kk = c(2,10,50,100,150,200,250,300,400,505)

for(i in kk){
  
  near = kknn(status_label~funding_rounds,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(funding_rounds,status_label,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  cat ("Press [enter] to continue")
  line <- readline()
}


plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

near = kknn(status_label~funding_rounds,train,test,k=20,kernel = "rectangular")

for(i in seq(1,505,by=100)){
  ii = near$C[i,1:20]
  plot(funding_rounds,status_label,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  abline(v=test[i,1],col=2,lty=2)
  points(funding_rounds[ii],status_label[ii],pch=19,col="blue")
  cat ("Press [enter] to continue")
  line <- readline()
}

######################################
## OUT-OF-SAMPLE Prediction
######################################

train = data.frame(funding_rounds,status_label)
test = data.frame(funding_rounds,status_label)

tr = sample(1:506,400)

train = train[tr,]
test = test[-tr,]

out_MSE = NULL

for(i in 2:350){
  
  near = kknn(status_label~funding_rounds,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  out_MSE = c(out_MSE,aux)
}


best = which.min(out_MSE)

plot(log(1/(2:350)),sqrt(out_MSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
text(log(1/best),sqrt(out_MSE[best])+0.3,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)


near = kknn(status_label~funding_rounds,train,test,k=42,kernel = "rectangular")

ind = order(test[,1])
plot(lstat,medv,main=paste("k=",42),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#########################################
# leave-one-out cross validation (LOOCV)#
#########################################

train = data.frame(funding_rounds,status_label)
test = data.frame(funding_rounds,status_label)


out_MSE = matrix(0,n,100)

for(j in 1:n){
  
  
  train_i = train[-j,]
  test_i = test[j,]
  
  for(i in 1:100){
    
    near = kknn(status_label~funding_rounds,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,2]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  cat(j,'\n')
}

mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)




#################################
# k-fold cross validation       #
#################################
kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(status_label~funding_rounds,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,2]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
  
}

mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)

