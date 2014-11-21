##########################
# 相关系数
##########################

# Step1: 利用data.frame来存储一组数据,注意里面有一个逗号分割,还有定义的时候用=而非<-
ore<-data.frame(
x = c(67, 54, 72, 64, 39, 22, 58, 43, 46, 34),
y = c(24, 15, 23, 19, 16, 11, 20, 16, 17, 13)
)

# Step2: 计算 样本的平均值(好像不能一起求)
mean(ore$x); mean(ore$y)

# Step3: 计算 样本的协方差
cov(ore)

# Step3: 验证 x与x的协方差就是x的方差
sd(ore$x)*sd(ore$x)
var(ore$x)
if (var(ore$x) == cov(ore)[1]){
  print("passed")
}

# Step4: 计算 样本的相关系数,你可以看到x与x的相关系数为1,x与y等价于y与x
#        而且返回的是一个矩阵,所以你如果要读取x与y的相关系数,就必须cor(ore)[2],好奇怪!!!
cor(ore)

# Step4: 验证 相关系数=协方差/标准差
cov(ore)[2]/(sd(ore$x)*sd(ore$y))




