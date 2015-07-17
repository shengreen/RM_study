rm(list = ls());
n <- 200;
x <- rnorm(n, mean = 0, sd = 1);
err <- rnorm(n, mean = 0, sd = 1);
y <-  4*sin(x) + exp(-x^2) + 0.5*err;

# mad:　给定一个向量，先求出中位数，再求出原向量的每一个元素与该中位数的距离，从而得到一个新向量（元素全为大于零的数）。再求这个新向量的中位数。
array(c('min(x)', 'max(x)','mean(x)','median(x)', 'sd(x)', 'mad(x)', min(x), max(x), mean(x), median(x), sd(x), mad(x)), dim=c(6,2))
array(c('min(y)', 'max(y)','mean(y)','median(y)', 'sd(y)', 'mad(y)', min(y), max(y), mean(y), median(y), sd(y), mad(y)), dim=c(6,2))
par(mfrow=c(2,2))
hist(x, col='VioletRed', main='hist of x')
boxplot(y, main='boxplot of y')

# KNN regression
knnfit1 <- knn.reg(x, NULL, y, k = 1);
plot(x, y, xlab='x', ylab='y hat', main='knn regression')
points(x, knnfit1$pred, col='green', pch='-')

knnfit2 <- knn.reg(x, NULL, y, k = 2);
points(x, knnfit2$pred, col='yellow', pch='*')

knnfit3 <- knn.reg(x, NULL, y, k = 40);
points(x, knnfit3$pred, col='pink', pch='#')

knnfit4 <- knn.reg(x, NULL, y, k = n-1);
points(x, knnfit4$pred, col='blue', pch='.')

legend("topleft", c("k=1", "k=2", "k=40", "k=n"), pch=c('-', '*', '#', '.'), lwd=c(1,1, 1, 1), col=c("green", "yellow", "pink", "blue"), title='legend')

# Kernal density regression
h1 = 0.5*n^(-0.2);
h2 = 1*n^(-0.2);
h3 = 2*n^(-0.2);
h4 = 4*n^(-0.2);
h = c(h1, h2, h3, h4)
kerfit1 <- array(dim=c(4, n))
kerfit2 <- array(dim=c(4, n))
kerfit3 <- array(dim=c(4, n))
kerfit4 <- array(dim=c(4, n))

for (hi in h){
  for (i in (1:n)){
    u <- (x-x[i])/hi;
    wgt1 = 3/4*(1-u^2)*(abs(u)<=1); # epanechnikov是不连续的抛物线函数,即二阶多项式函数
    wgt2 = 15/16*(1-u^2)^2*(abs(u)<=1); # quartic 四阶多项式函数
    wgt3 = (2*pi)^(-0.5)*exp(-u^2/2);
    kerfit1[hi,i] = sum(y*wgt1)/sum(wgt1);
    kerfit2[hi,i] = sum(y*wgt2)/sum(wgt2);
    kerfit3[hi,i] = sum(y*wgt3)/sum(wgt3);
  }
}
# plot(density(x, kernel = c("epanechnikov"), n = n))
