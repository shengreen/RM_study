rm(list = ls());
n = 30;
varnum = 10;
x <- array(rnorm(n*varnum), dim=c(n, varnum));
beta <- array(c(1, 0.8, 0.6, 0.4, 0.2, 0, 0, 0, 0, 0), dim=c(varnum, 1))
y <- x%*%beta + array(rep(rnorm(n), 1), dim=c(n, 1))

#############################################
#
#############################################
x1 <- scale(x, center=TRUE, scale=FALSE)
y1 <- scale(y, center=TRUE, scale=FALSE)
beta_hat1 <- solve(var(x1))%*%cor(x1, y1)
temp <- apply(x1, 2, mean)
beta0_hat1 <- mean(y1)-t(temp)%*%beta_hat1
eps_var1 <- var(beta_hat1%*%x1 + beta0_hat1 - y1)
plot(x1%*%beta_hat1 + beta0_hat1 - y1)

#############################################
#
#############################################
x2 <- cbind(array(rep(1), dim=c(n,1)), x)
beta_hat2 <- solve(t(x2)%*%x2)%*%(t(x2)%*%y)
eps_var2 <- var(x2%*%beta_hat2 - y)
plot(x2%*%beta_hat2 - y)

#############################################
#
#############################################
lm.fit <- lm(y~x)
eps_var3 <- lm.fit.
