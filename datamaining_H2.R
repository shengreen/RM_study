rm(list = ls());
n = 30;
varnum = 10;
x <- array(rnorm(n*varnum), dim=c(n, varnum));
beta <- array(c(1, 0.8, 0.6, 0.4, 0.2, 0, 0, 0, 0, 0), dim=c(varnum, 1))
y <- x%*%beta + array(rep(rnorm(n), 1), dim=c(n, 1))

#############################################
#
#############################################
xx <- scale(x, center=TRUE, scale=FALSE)
yy <- scale(y, center=TRUE, scale=FALSE)
beta_hat1 <- solve(var(xx))%*%cor(xx, yy)
temp <- apply(x, 2, mean)
beta0_hat1 <- mean(y)-t(temp)%*%beta_hat1
