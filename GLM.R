
#Reading in the Data
df_inv = read.csv(file = '/Users/SameerAhmed/Documents/GitHub/vcsuccess/StartupSuccess.csv')

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
logistic_fit= glm(status ~ city + funding_rounds + total_investment,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ funding_rounds + total_investment,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment, data=df_inv_Train,family='binomial')
summary(logistic_fit)


##4273
logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~  funding_rounds  + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

prob_success_pred=predict(logistic_fit,df_inv_Test,type="response")

prob_success_pred

pred_success = ifelse(prob_success_pred>.90,"success","closed")
table(df_inv_Test$status, pred_success)

pred_success
df_inv_Test$status

mean(pred_success==df_inv_Test$status)
mean(pred_success!=df_inv_Test$status)



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
