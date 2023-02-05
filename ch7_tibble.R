rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# tibble package
################################################################################
library(tidyverse)
# tibble其实是data frame的一种变体，提供了更为方便的使用，可以与data frame互相转换
as_tibble(iris)
tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y 
) # 这里有点类似于Python中的生成器，tibble函数会自动重复长度为1的输入（也就是单个输入）
# 相较于frame来说，tibble功能更少，不能改变输入类型、变量名，也不能进行行命名
tribble(
  ~x, ~y, ~z,
  'a', 1, 2,
  'b', 3, 4
) # 这是tibble的转置形式，可以对少量数据进行按行输入，第一行用特殊格式表明是标题行

###############tibble/data.frame比较###############
# 改进了打印的输出
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = T)
)
nycflights13::flights %>% print(n = 30, width = Inf) # Inf表示打印所有列
# 取子集
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df$x # 这样取出来是向量
df[1] # 可以发现，这样取出来的仍然是一个tibble
df[[1]] # 而这样就是一个单纯的向量
df %>% .$x # 需要在管道中使用，那就用.作为占位符

a <- df[[1]]
b <- enframe(a, name = 'test name', value = 'test value') # 可以将list或者向量转化为frame，反过来则是deframe
var <- 'test name'
# 将变量名保存到一个变量中，该如何取出相应的数据框
b[var]
b[[var]]

# 小测
df_test <- data.frame(abc = 1, xyz = 'a')
tb_test <- tibble(abc = 1, xyz = 'a')
df_test[, 'xyz']
df_test[, c('xyz', 'abc')]
df_test[1, 2]


