
#Reading in the Data
df_inv = read.csv(file = '/Users/SameerAhmed/Documents/GitHub/vcsuccess/StartupSuccessOverSampled.csv')

#changing it to two factor
df_inv$status[df_inv$status == 'acquired'] <- 'success' 
df_inv$status[df_inv$status == 'operating'] <- 'success'

df_inv$status = as.factor(df_inv$status)

df_inv$region = as.factor(df_inv$region)
df_inv$status = as.factor(df_inv$status)



##getting test data 
data_set_size = nrow(df_inv) 
test_set_size = round(0.20*data_set_size) #I want a 20% validation set.
RNGkind(sample.kind = "Rounding")
set.seed(8239) 
tickets = sample(data_set_size,test_set_size)
df_inv_Test = df_inv[tickets,]
df_inv_Train = df_inv[-tickets,]




library(leaps)
build = regsubsets(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label, data = df_inv_Train, method = "backward", nvmax=6)
plot(build, scale="r2")

#different variable choice


logistic_fit= glm(status ~ funding_rounds + total_investment,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment, data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~  funding_rounds  + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + seed_label + venture_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)



prob_success_pred=predict(logistic_fit,df_inv_Test,type="response")

prob_success_pred

pred_success = ifelse(prob_success_pred>.50,"success","closed")
table(df_inv_Test$status, pred_success)


install.packages('caret')
library(caret)

confusionMatrix(df_inv_Test$status, pred_success, mode = "everything")


pred_success
df_inv_Test$status

mean(pred_success==df_inv_Test$status)
mean(pred_success!=df_inv_Test$status)


##ploting lift
y = as.numeric(default)-1
ns=1000
sv = seq(from=.0,to=.99,length.out=ns)
FP=rep(0,ns)
TP=rep(0,ns)
N=rep(0,ns)
n0=sum(y==0)
for(i in 1:ns) {
  N[i] = sum(prob_success_pred>sv[i])/length(y)
  TP[i] = sum((prob_success_pred>sv[i]) & (y==1))/sum(y==1)
  FP[i] = sum((prob_success_pred>sv[i]) & (y==0))/sum(y==0)
}

par(mfrow=c(1,3))
par(mai=c(0.9,0.9,.4,.4))
plot(sv,N,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. N',cex.main=2)
plot(sv,TP,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. TP',cex.main=2)
plot(sv,FP,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. FP',cex.main=2)

par(mai=c(0.9,0.9,.4,.4))
par(mfrow=c(1,2))
plot(FP,TP,type='l',col='blue',cex.lab=2.0)
abline(0,1,lty=2)
title(main='ROC',cex.main=2)
plot(N,TP,type='l',col='blue',cex.lab=2.0)
abline(0,1,lty=2)
title(main='Lift',cex.main=2)
temp = liftf(y,prob_success_pred,dopl=FALSE)
lines((1:length(y))/length(y),temp,col='red',lty=3)


temp = liftf(y,prob_success_pred)


#another attempt

install.packages("pROC")
library(pROC)

install.packages('ROCR')
library(ROCR)

ROCR_pred_test <- prediction(prob_success_pred,df_inv_Test$status)

ROCR_perf_test <- performance(ROCR_pred_test,'tpr','fpr')

plot(ROCR_perf_test,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

auc_value <- roc(df_inv_Test$status, prob_success_pred)
auc(auc_value)

###
Prob_closed_in_data = mean(df_inv_Train == 'closed')
Prob_closed_in_data



true_prob_closed = 0.15
new_beta0 = logistic_fit$coefficients[1] + log(true_prob_closed/(1-true_prob_closed)) - log(Prob_closed_in_data/(1-Prob_closed_in_data))

logistic_fit$coefficients[1]
new_beta0
prob_yes[1]

logistic_fit$coefficients[1] = new_beta0
pred_yes_new_beta0 = predict(logistic_fit,type='response')
pred_yes_new_beta0[1]

pred_yes_new_beta0 = predict(glm1,heart,type='response')
pred_yes_new_beta0[1]
