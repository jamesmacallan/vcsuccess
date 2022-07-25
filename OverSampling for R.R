rm(list=ls())

df_inv <- read.csv(file = 'C:/Users/apurv/OneDrive/Desktop/StartupSuccess1.csv')

#Installing package ROSE for imbalanced data oversampling
#install.packages("ROSE")
library(ROSE)

library(rpart)
#over sampling
table(df_inv$status_label)

data_balanced_over <- ovun.sample(status_label ~ ., data = df_inv, method = "over",N = 24190)$data
table(data_balanced_over$status_label)

#Checkpoint for Cleaning
write.csv(data_balanced_over,"C:/Users/apurv/OneDrive/Desktop/StartupSuccessOverSampled.csv", row.names = TRUE)
