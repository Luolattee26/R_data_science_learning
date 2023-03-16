rm(list = ls())
setwd('~/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use modelr to build basic models
# all used is linear model
################################################################################


library(tidyverse)
library(modelr)
options(na.action = na.warn)

ggplot(sim1, mapping = aes(x, y)) + geom_point()
# 可以发现，sim1这玩意就是个简单的线性数据集
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
) # 这里其实就是机器学习里面的模型构建
# 可以用abline来体现一下，我们构建的模型
ggplot(sim1, mapping = aes(x, y)) + geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()
# abline这个函数其实就是专门用来画kx+b的，需要指定k/b分别是啥
# 可以发现，我们构建的模型有很多都是垃圾，完全没用，需要筛选（其实也就是ML里面的策略和算法）

# 为了筛选，我们需要构建一个函数来进行模型“输入输出”的过程
model <- function(a, data){
  a[1] + data$x * a[2]
}
model(c(7, 1.5), sim1) # 这样我们就得到了我们的全体输出值，这里是一个模型/预测的
# 接下来，再构建一个函数，来得到我们的均方根误差
calc_diff <- function(data, mod_parm){
  diff <- data$y - model(mod_parm, data)
  sqrt(mean(diff ^ 2))
}
calc_diff(sim1, c(7, 1.5)) # 这样也得到了一次预测中，误差值
# 最后则是利用map，为我们构造的所有模型计算误差
all_diff <- function(a1, a2){
  calc_diff(sim1, c(a1, a2))
}
(models <- models %>% mutate(error = map2_dbl(a1, a2, all_diff)))
# 将表现最好的10个模型进行可视化
ggplot(sim1, mapping = aes(x, y)) + geom_abline(aes(intercept = a1, slope = a2, color = -error), data = filter(models, rank(error) <= 10)) +
  geom_point(color = 'grey')
ggplot(models, mapping = aes(a1, a2)) + geom_point(aes(color = -error)) +
  geom_point(data = filter(models, rank(error) <= 10), color = 'red') # 这两种模型可视化方法都是可行的

# 除了上面这种随机得到模型的方法，还有一种更可靠的方法是在一个平均分布的群体中
# 比如这样得到a1参数的备选
a1 <- seq(-5, 20, length = 25)

# 还有一种牛顿提出的迭代优化办法
# 直观的描述就是从起点开始，寻找一个偏离值最大的，而后减小一点点，直到偏离值不能减少
best <- optim(c(0, 0), calc_diff, data = sim1)
# 这个函数语法也很简单，起点+计算偏离值的函数+训练集
best$par
# 当然，对这种线性数据集，直接用lm线性拟合是最方便的，自己写当然也是可以的啦
(sim1_bestmodel <- lm(y ~ x, data = sim1))

# 可以计算残差，输入你需要用到的模型即可
sim1 %>% add_residuals(sim1_bestmodel)

# 在R中，我们常常会看到使用公式来构建模型，比如lm中
# 使用model_matrix可以看到R是如何把公式转化成具体的模型的
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df, y ~ x1)
# 这个输出是按照如下格式来的
# y=a1*out1+a2*out2
# 这个例子中，则是代表x1有两种取值2/1，乘上一个模型中的系数a2，加上前面默认的截距1作为out1乘以a1
model_matrix(df, y ~ x1 - 1) # 默认是一定带有截距项的，如果不想要，可以在公式中减去1

# 对于分类变量来说，R通常会自动的将其转为0/1来进行建模处理
df <- tribble(
  ~sex, ~response,
  'male', 1,
  'female', 2,
  'male', 1
)
model_matrix(df, response ~ sex)
# 可以发现，R自动生成了一个sexmale变量，相当于把原来的字符型变量sex转化为了数字
# 其实理论上是还可以生成一个sexfemale变量的，但是这样等于增加了变量数量，非常容易造成过拟合

# 在实际的模拟过程中，我们常常会使用交互项，也就是两个自变量不再独立，比如相乘
ggplot(sim3, mapping = aes(y, x1)) + geom_point(aes(color = x2))
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
# 使用了交互项之后，交互项每一个组成部分都要被包括在模型当中，如下
model_matrix(sim3, y ~ x1 * x2)
# 想要可视化这种含有交互项的模型，需要特殊的方法
(grid <- sim3 %>% data_grid(x1, x2) %>% gather_predictions(mod1, mod2))
# data_grid函数会自动的寻找传入变量的每个唯一值，并且将其组合起来
ggplot(sim3, aes(x1, y, color = x2)) + geom_point() + geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model) # 可以发现，两个模型的差别非常大
# 从两种模型的残差图也可以发现，mod1明显在x2=b的预测上是错误的
sim3 <- sim3 %>% gather_residuals(mod1, mod2)
ggplot(sim3, aes(x1, resid, color = x2)) + geom_point() + facet_wrap(model ~ x2)

# 和上面类似的，我们也可以使用两个连续变量来构造交互项
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
(grid <- sim4 %>% data_grid(
  x1 = seq_range(x1, 5),
  x2 = seq_range(x2, 5)
) %>% gather_predictions(mod1, mod2))
ggplot(grid, aes(x1, x2)) + geom_tile(aes(fill = pred)) + facet_wrap(~ model)
# 这样的可视化显然看不太出预测的结果
ggplot(grid, mapping = aes(x1, pred, color = x2, group = x2)) +
  geom_line() + facet_wrap(~ model)
ggplot(grid, aes(x2, pred, color = x1, group = x1)) + geom_line() + 
  facet_wrap(~ model) # 这样可以发现，不管是不是连续变量，交互项都是差不多的样子

# 在构造模型的时候，我们常常会对变量进行转换，比如自变量设置为x^2
# 在模型公式中，此类转换必须要用I()进行包装，不然R会认为这是公式的一部分
df <- tibble(
  x = c(1, 2, 3),
  y = c(1, 4, 9)
)
model_matrix(df, y ~ x^2 - 1)
model_matrix(df, y ~ I(x^2) - 1)
# 可以发现，第一个模型仍然是将x作为自变量，这是因为R把^2认为是公式的一部分，也就是x*x，x自己的交互项
# R会自动避免过剩的无用变量，x与自己的交互项，那自然会被认为是x自身

# 模型变量转换的一个很有用的地方是，可以利用泰勒公式来近似任何的非线性函数，这样我们就能通过一个线性公式来表示非线性关系
df <- tribble(
  ~x, ~y,
  1, 1,
  2, 2,
  3, 3
)
model_matrix(df, y ~ poly(x, 2)) # 因为泰勒手打很烦，R提供了ploy函数来快捷输入，后面的2表示两项
# poly容易造成多项式趋近无穷，建议用下面的splines::ns()
model_matrix(df, y ~ ns(x, 2))

# 用三角函数来看一下效果
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 100),
  y = 4 * sin(x) + rnorm(length(x))
) # 加了扰动项的三角函数
ggplot(sim5, aes(x, y)) + geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% data_grid(x = seq_range(x, n = 50)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = 'y') # 为了后面画图方便，把预测值名字改成y

ggplot(sim5, aes(x, y)) + geom_point() + geom_line(data = grid, color = 'red') +
  facet_wrap(~ model)
# 很明显看到，当复杂度增加到5的时候，得到的预测结果最好

# 通过设定na.action，可以决定对NA如何处理
options(na.action = na.exclude) # 直接忽略，不加入模型中
options(na.action = na.warn) # 给一个警告
nobs(mod5) # 看模型用了多少观测值进行训练

