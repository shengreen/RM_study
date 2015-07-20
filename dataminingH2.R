rm(list = ls());
n = 30;
varnum = 10;
x <- array(rnorm(n*varnum), dim=c(n, varnum));
beta <- array(c(1, 0.8, 0.6, 0.4, 0.2, 0, 0, 0, 0, 0), dim=c(varnum, 1))
y <- x%*%beta + array(rep(rnorm(n), 1), dim=c(n, 1))

par(mfrow=c(2,2)) 

###############################################
# Centralize
###############################################
x <- x - apply(x, 2, mean)
y <- y - mean(y)

################################################
# Method 1: Use Var and Cov to resolve
################################################
beta_hat1 <- solve(var(x))%*%cov(x, y)
beta0_hat1 <- mean(y)-apply(x, 2, mean)%*%beta_hat1
residual1 <- y - x%*%beta_hat1 - array(rep(1:1), dim=c(30,1))%*%beta0_hat1
eps_var1 <- var(residual1)
plot(residual1, main=paste('method1 \n(residual var = ',eps_var1, ')'))

################################################
# Method 2: Use matrix manipulation to resolve
################################################
x2 <- cbind(array(rep(1), dim=c(n,1)), x)
beta_hat2 <- solve(t(x2)%*%x2)%*%(t(x2)%*%y)
residual2 <- y - x2%*%beta_hat2
eps_var2 <- var(residual2)
plot(residual2, main=paste('method2 \n(residual var = ',eps_var2, ')'))

#################################################
# Method 3: Use R's lm function to do regression
#################################################
lm.fit <- lm(y~x)
summary(lm.fit)
coefficients(lm.fit)
confint(lm.fit, level=0.95)
fitted(lm.fit)
plot(residuals(lm.fit), main=paste('method3 \n(residual var = ',var(residuals(lm.fit)), ')'))
#plot(lm.fit)
