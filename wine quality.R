# data ------------------------------------------------------------------
red['color'] <- 'red'
white['color'] <- 'white'
data <- rbind(red, white)

# load library ------------------------------------------------------------------
library(MASS) # for polr
library(car) # for vif
library(glmnetcr) # for variable selection
library(glmnet)

# ordinal regression fitting ------------------------------------------------------------------
# red
m1 <- polr(as.factor(quality) ~ ., data = red[1:12], Hess = TRUE)
summary(m1)
# white
m2 <- polr(as.factor(quality) ~ ., data = white[1:12], Hess = TRUE)
summary(m2)
# all
m3 <- polr(as.factor(quality) ~ ., data = data[1:12], Hess = TRUE)
summary(m3)

# collinearity
vif1 <- vif(m1)
vif2 <- vif(m2)
vif3 <- vif(m3)

# removing collinearity fitting
# red
m1.1 <- polr(as.factor(quality) ~ fixed.acidity + volatile.acidity + citric.acid +
               residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol,
             data = red[1:12], Hess = TRUE)
vif1.1 <- vif(m1.1)

# white
m1.2 <- polr(as.factor(quality) ~ fixed.acidity + volatile.acidity + citric.acid +
               residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol,
             data = white[1:12], Hess = TRUE)
vif1.2 <- vif(m1.2)
# all
m1.3 <- polr(as.factor(quality) ~ fixed.acidity + volatile.acidity + citric.acid +
               residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol,
             data = data[1:12], Hess = TRUE)
vif1.3 <- vif(m1.3)

# variable selection ------------------------------------------------------------------
# logit ------------------------------------------------------------------
# red
y <- red$quality
x <- cbind(red[1:11])
fit1 <- glmnetcr(x, y)
fit1[["beta"]]
# plot(fit1, xvar = "step", type = "bic")
# plot(fit1, xvar = "step", type = "coefficients")
m5 <- polr(as.factor(quality) ~ fixed.acidity+volatile.acidity+chlorides+total.sulfur.dioxide+sulphates+alcohol,
           data = red[1:12], Hess = TRUE)
ctable <- round(coef(summary(m5)), 4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))

# white
y <- white$quality
x <- cbind(white[1:11])
fit2 <- glmnetcr(x, y)
fit2[["beta"]]
m6 <- polr(as.factor(quality) ~ fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+alcohol,
           data = red[1:12], Hess = TRUE)
ctable <- round(coef(summary(m6)), 4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))

# probit ------------------------------------------------------------------
# red
y <- red$quality
x <- cbind(red[1:11])
fit1 <- glmnetcr(x, y)
fit1[["beta"]]
# plot(fit1, xvar = "step", type = "bic")
# plot(fit1, xvar = "step", type = "coefficients")
m7 <- polr(as.factor(quality) ~ fixed.acidity+volatile.acidity+chlorides+total.sulfur.dioxide+sulphates+alcohol,
           data = red[1:12], Hess = TRUE)
ctable <- round(coef(summary(m7)), 4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))

# white
y <- white$quality
x <- cbind(white[1:11])
fit2 <- glmnetcr(x, y)
fit2[["beta"]]
m6 <- polr(as.factor(quality) ~ fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+alcohol,
           data = red[1:12], Hess = TRUE)
ctable <- round(coef(summary(m6)), 4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))





# goodness of fit in ordinal logistic regression models ------------------------------------------------------------------


# model assumptions checking ------------------------------------------------------------------



# causal effects ------------------------------------------------------------------
# red
results = matrix(nrow = 15, ncol = 4)

newred = cbind('fixed.acidity' = red$fixed.acidity,
               "volatile.acidity" = red$volatile.acidity,
               "chlorides" = red$chlorides,
               "total.sulfur.dioxide" = red$total.sulfur.dioxide,
               "sulphates" = red$sulphates,
               "alcohol" = red$alcohol,
               "quality" = red$quality)

for (i in 1:6) {
  for (j in i+1:6) {
    m5 <- polr(as.factor(quality) ~ newred[,i], data = newred, Hess = TRUE)
    summary(m5)
    results[1,i] <- coef(m5)
    m6 <- polr(as.factor(quality) ~ newred[,j], data = newred, Hess = TRUE)
    summary(m6)
    results[2,i] <- coef(m6)
    m7 <- polr(as.factor(quality) ~ newred[,i] + newred[,j], data = newred, Hess = TRUE)
    summary(m7)
    results[3:4,i] <- coef(m7)
  }
}





