#R project - Intro to ML

# lasso regression 
library(readr)

df_inv = read_csv("StartupSuccess.csv")

data_set_size = nrow(df_inv)
test_set_size = round(0.30*data_set_size)

df_inv$status = as.factor(df_inv$status)

df_inv$region = as.factor(df_inv$region)
df_inv$status = as.factor(df_inv$status)

lm.fit = lm(status_label~  total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, data = df_inv)
summary(lm.fit)

library(glmnet)
set.seed(1)
library(dplyr)
train = sample(1:nrow(df_inv),nrow(df_inv)/2)
test=(-train)
y.test = y[test]

x = model.matrix(status_label ~ + market_type + total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, data = df_inv)[, -1]
y = df_inv$status_label + df_inv$total_investment_cde + df_inv$diff_funding_year_cde + df_inv$funding_rounds_cde + df_inv$seed_cde + df_inv$venture_cde + df_inv$market_type

cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)

cv.out$lambda.min

lasso.startup = glmnet(x[train,], y[train], alpha = 1, thresh = 1e-12, lambda = grid)
grid = 10^seq(10, -2, length = 100)
plot(lasso.startup)
summary(lasso.startup)
lasso.pred = predict(lasso.startup, s=cv.out$lambda.min, newx = x[test, ])
sqrt(mean((lasso.pred - y.test)^2))


out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = 'coefficients', s=cv.out$lambda.min)[1:7,]
lasso.coef

# ridge regression 
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
plot(ridge.mod)
dim(coef(ridge.mod))
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
cv.out$lambda.1se
cv.out$lambda.min
ridge.pred = predict(cv.out, s = cv.out$lambda.min, newx = x[test, ], type = 'coefficients')[1:7,]
sqrt(mean((ridge.pred - y.test)^2))
ridge.pred

# PCR
library(pls)
set.seed(3) 
pcr.fit = pcr(status_label ~ total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, data = df_inv, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)
pcr.pred = predict(pcr.fit, x[test, ], ncomp = 10)
mse.pcr = mean((pcr.pred-y.test)^2)
mse.pcr

# PLS 
set.seed(1) 
pls.fit = plsr(status_label ~ total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, data = df_inv, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred = predict(pls.fit, x[test, ], ncomp = 6)
mean((pls.pred - y.test)^2)

# Subset selection 
library(ISLR2)
View(df_inv)
sum(is.na(df_inv))

install.packages("leaps")
library(leaps)
regfit.full = regsubsets(status_label ~ total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, df_inv, nvmax = 13)
reg.summary = summary(regfit.full)

# plotting RSS, R2, Cp, and BIC 
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS")
which.min(reg.summary$rss)

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq")
which.max(reg.summary$adjr2)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC")
which.min(reg.summary$bic)
