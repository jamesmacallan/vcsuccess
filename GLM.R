#Reading in the Data
##needs to be re-read 
df_inv = read.csv(file = '/Users/SameerAhmed/Documents/GitHub/vcsuccess/StartupSuccessOverSampled.csv')

#changing it to two factor (re run this again)
df_inv$status[df_inv$status == 'acquired'] <- 'success' 
df_inv$status[df_inv$status == 'operating'] <- 'success'

df_inv$status = as.factor(df_inv$status)
df_inv$region = as.factor(df_inv$region)
df_inv$status = as.factor(df_inv$status)

max(df_inv$total_investment)
which(df_inv$total_investment == 30079503000)

#removing Verizon
df_inv = df_inv[-c(11243), ]


##getting test data 
data_set_size = nrow(df_inv) 
test_set_size = round(0.30*data_set_size) #I want a 20% validation set.
RNGkind(sample.kind = "Rounding")
set.seed(8239) 
tickets = sample(data_set_size,test_set_size)
df_inv_Test = df_inv[tickets,]
df_inv_Train = df_inv[-tickets,]


#regsubset
library(leaps)
build = regsubsets(status ~ diff_funding_year + market_type + funding_rounds + total_investment_cde + seed_label + venture_label + funding_rounds_label, data = df_inv_Train, method = "backward", nvmax=6)
plot(build, scale="r2")

#different variable choice
logistic_fit= glm(status ~ funding_rounds + total_investment_cde,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment, data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~  funding_rounds  + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ diff_funding_year + market_type + funding_rounds + seed_label + total_investment_cde,data=df_inv_Train,family='binomial')
summary(logistic_fit)



prob_success_pred=predict(logistic_fit,df_inv_Test,type="response")

pred_success = ifelse(prob_success_pred>.55,"success","closed")
table(df_inv_Test$status, pred_success)

install.packages('caret')
library(caret)

confusionMatrix(as.factor(df_inv_Test$status), as.factor(pred_success), mode = "everything")

mean(pred_success==df_inv_Test$status)
mean(pred_success!=df_inv_Test$status)


##ploting lift

install.packages("pROC")
install.packages('ROCR')

library(ROCR)
library(pROC)


ROCR_pred_test <- prediction(prob_success_pred,df_inv_Test$status)

ROCR_perf_test <- performance(ROCR_pred_test,'tpr','fpr')

plot(ROCR_perf_test,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
abline(0,1,lty=2)


auc_value <- roc(df_inv_Test$status, prob_success_pred)
auc(auc_value)

#### Attempt at changing beta for inbalanced dataset - failed attempt
#Prob_closed_in_data = mean(df_inv_Train == 'closed')
#Prob_closed_in_data



#true_prob_closed = 0.15
#new_beta0 = logistic_fit$coefficients[1] + log(true_prob_closed/(1-true_prob_closed)) - log(Prob_closed_in_data/(1-Prob_closed_in_data))

#logistic_fit$coefficients[1]
#new_beta0
#prob_yes[1]

#logistic_fit$coefficients[1] = new_beta0
#pred_yes_new_beta0 = predict(logistic_fit,type='response')
#pred_yes_new_beta0[1]

#pred_yes_new_beta0 = predict(glm1,heart,type='response')
#pred_yes_new_beta0[1]
