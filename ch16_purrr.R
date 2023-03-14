rm(list = ls())
setwd('~/Desktop/R_data_science_learning/')
getwd()


################################################################################
# Purrr
################################################################################


library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
# 在这里写了一个计算中位数的函数，可以发现，我们先定义了一个输出结果
# 在此处使用vector定义了一个输出向量，这样我们在函数的循环过程中就可以直接“填空”
# 如果按照以前的做法，每次新生成一个向量，那么函数的运算速度会非常非常慢
calc_median <- function(df){
  output <- vector(mode = 'double', length = ncol(df))
  for(i in (1:ncol(df))){
    output[[i]] = median(df[[i]])
  }
  return(output)
}
res <- calc_median(df)
# 对于for循环来说，还存在许多的变体
# 比如如果我们相对数据进行修改，而不是生成新的数据
# e.g.,normalization

calc_normalization <- function(lst){
  size_factor <- median(lst)
  for(i in lst){
    lst[i] <- i/size_factor
  }
  print(lst)
}
calc_normalization(c(1, 5, 3, 2, 4))

# 通常，在for循环中我们会用元素来进行循环，比如上面的(i in list)
# 但是这种循环模式并不适用所有情况
# 也可以使用名称进行循环，(nm in names(x))
# 这种方式会给出一个名称，从而使用x[[nm]]来访问我们想要的数据，这在画图之类的上面十分方便
# 与元素循环不一样的是，我们需要特别的方法来定义名称循环的输出向量
res <- vector('list', length(x))
names(res) <- names(x)

# 有时候在for循环中，我们并不知道输出的长度是多少
# 当然可以在循环当中采取不断合并的操作，但是这样每次循环都需要复制一遍上次的数据
# 可以使用列表来储存这些变化的数据，最后在循环外进行一次性的合并
means <- c(0, 1, 2)
output <- vector('list', length(means))
for(i in length(means)){
  n <- sample(100, 1)
  output[[i]] <- rnorm(n, means[[i]])
}
unlist(output)
purrr::flatten_dbl(output) # 两种方法是等价的
# 这种先生成列表再合并的方式，对于字符串以及数据框都是一样的

# R语言是一种函数式的语言，这一特点的最好体现就是在R里面可以非常方便的构造函数来包装代码
# 同时也可以将函数作为参数传入新的函数中
# 比如写一个可以进行不同数学处理的函数
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10)
)
statistic <- function(df, fun){
  output <- vector('double', ncol(df))
  for(i in (1:ncol(df))){
    output[[i]] <- fun(df[[i]])
  }
  print(output)
}
print(df)
statistic(df, mean)
statistic(df, median)
# 这里的第二个参数其实就是个函数，把相应的函数传进去了

# 除了for循环外，想实现循环操作，各种map函数也是非常好的选择
# map函数接受两个参数，第一个参数一定是一个向量（广义上），第二个则是需要进行的函数操作
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10)
)
df %>% map_dbl(mean) # 可以发现，自动的将每一列作为向量进行了循环
# 相似的col_summary函数也能实现类似的功能，但是map函数是C写的，速度快非常多

# 在map函数中，所需传入的处理函数也可以自己写
models <- mtcars %>% split(.$cyl) %>% 
  map(function(df){
  lm(mpg ~ wt, data = df)
}) # 这里其实就是，用cyl作为依据将mtcars进行拆分，而后对每个部分进行mpg/wt的线性回归
# 但是可以发现，这样写函数其实是很麻烦的，不像python里面可以用lambda来写隐式函数
# 在purrr包中，提供了一种叫单侧公式的方法来解决
models <- mtcars %>% split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .)) # 一方面用.来进行指代，另一方面用~来表示需要进行处理的函数
# 在提取结果里面也十分有用
models %>% map(summary) %>% map(~.$r.squared) # 可以发现，很方便的就提出来了，而且代码很简洁
# 同样的，按照位置来选择元素也是可以的
df <- tibble(
  a = c(1:4),
  b = c(1:4)/10
)
df %>% map(~.[2])
df %>% map(2) # 这两种方法是等价的，但是显然上面的那个可读性更好

# 当进行各种函数处理的时候，常常会出现error，此时可以通过safely函数来看错误的情况
safe_log <- safely(log)
str(safe_log(10))
# safely函数需要传入一个函数作为对象，返回一个2元素的列表
# 当传入的函数正确运行的时候，传出列表中result为结果，error则为NULL，反之也是成立的
# 通常，我们会将map和safely合并在一起使用
x <- list(10, 220, 330, 430, 512, 'a')
y <- x %>% map(safely(log))
str(y)
# 通过transpose函数来将所有的result汇聚在一起
y <- y %>% transpose()
str(y)
# 通过筛选操作，当数据量很大的时候，我们就能知道原数据哪里出问题了
whether_ok <- y$error %>% map_lgl(is_null)
x[!whether_ok] # 这里就显示出来a这个元素是错的

# 普通的map函数都是对单个输入进行迭代，而map2和pmap可以对2/多个输入对象进行映射迭代
mu <- list(5, 3, 10)
sigma <- list(1, 2, 3)
map2(mu, sigma, rnorm, n = 3) %>% str()
# 指定均值和标准差，分别得到三个符合正态分布的观测值

# 按照这个逻辑，可以开发map3等等，所以提供了pmap，接受一个列表作为参数
mu <- list(5, 3, 10)
sigma <- list(1, 2, 3)
n <- list(2, 4, 6)
# 这里的意思是我想生成三列不同的正太随机数，数量分别为246，均值和方差也不同
args1 <- list(n, mu, sigma)
args1 %>% pmap(rnorm) %>% str() # 其实就是传入了一个“参数列表”
# 这里要注意的是，如果直接这么写args，一定要按照rnorm函数相应的顺序来写，顺序改变，那得到的结果也会改变
# 所以更加合适的做法是，写args时标明参数的对应关系
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% pmap(rnorm) %>% str() # 这样就算顺序改变了，得到的结果也是正确的结果


# 在前面的章节，介绍了游走函数这么一个概念，游走函数其实就是更注重函数过程中发生的操作，而不在乎结果
# 比如save，就是这么一个游走函数
walk()
walk2()
pwalk()
# 和map其实就是完全一样的，这在保存数据的时候非常有用
library(ggplot2)
plots <- mtcars %>% split(.$cyl) %>% map(~ggplot(., mapping = aes(x = mpg, y = wt)) +
  geom_point())
paths <- stringr::str_c(names(plots), '.pdf')
pwalk(list(path, plots), ggsave, path = getwd())
# 这样就很方便的实现了多张图的批量保存
# 这里要注意list(path, plots)，由于我们为了简洁，没有写出参数的对应关系，所以顺序一定要正确


# 除了map以及walk函数之外，purrr中的reduce也可以起到一定的“降维”作用
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(nane = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
) # 这里其实可以认为我们有很多个数据框
age = tibble(name = "John", age = 30)
sex = tibble(nane = c("John", "Mary"), sex = c("W", "F"))
trt = tibble(name = "Mary", treatment = "A")
# 这样效果是一样的
# 通过reduce，我们可以进行合并操作
dfs %>% reduce(cross_join)
# reduce函数接收一个二元函数作为参数，将其应用在整个列表上，直到目标列表只剩一个元素
# 比如这里就是将full_join不停循环应用在dfs上

# 或者是找出一堆数据的交集
num <- list(
  c(1:3),
  c(1:4),
  c(1:5)
)
num %>% reduce(intersect)


