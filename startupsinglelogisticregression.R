#Boxplots
df <- read.csv(file = "C:/Users/jmand/Downloads/StartupSuccessOverSampled.csv")
first = boxplot(df$total_investment~df$status_label, outline=FALSE)
second = boxplot(df$funding_rounds~df$status_label, outline=FALSE)
third = boxplot(df$venture~df$status_label, outline=FALSE)
fourth = boxplot(df$seed~df$status_label, outline=FALSE)
fifth = boxplot(df$diff_funding_year~df$status_label, outline=FALSE)

#Creation of Training and Test Sets
data_set_size = nrow(df)
print(data_set_size)
test_set_size = round(0.30*data_set_size)
print(test_set_size)
RNGkind(sample.kind = "Rounding")
set.seed(8239)
tickets = sample(data_set_size,test_set_size)
df_Test = df[tickets,]
df_Train = df[-tickets,]
df_Train <- na.omit(df_Train)
df_Test <- na.omit(df_Test)

#Single Variable Logistic Fit
logistic_fit= glm(status_label ~ funding_rounds, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ total_investment, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ venture, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ seed, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ diff_funding_year, data=df_Train,family='binomial')
summary(logistic_fit)

#Attempt to fit probability vs eta. Our inability to do so made us scrap this model
#from our final project. It is clear here that there isn't a properly defined eta,
#and the graph is interpreting the code as something to draw a line graph of. 
z = seq(from=-5,to=5,length.out=1000)
Fz = exp(z)/(1+exp(z))
plot(z,Fz,type='l',col='blue',lwd=2.5,xlab=expression(eta),ylab='F',cex.lab=1.3)
eta = predict(logistic_fit)
pyx = predict(logistic_fit,type='response')
plot(eta,pyx,type='l',col='blue',lwd=2.5,xlab=expression(eta),ylab='F',cex.lab=1.3)


