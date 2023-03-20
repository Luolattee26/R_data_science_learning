rm(list = ls())
setwd('C:/Users/Administrator/Desktop/R_data_science_learning/')
setwd('~/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use purrr&broom to solve multiple models
################################################################################


library(tidyverse)
library(modelr)


# 在R中构建df的时候，其实是将df的每一列作为一个list来进行处理的
data.frame(
  x = list(1:3, 3:5)
) # 在这里，R将我们传入的列表视为了col的列表，1:3是col1，3:5是col2
data.frame(
  x = I(list(1:3, 3:5))
) # 和前面一样的，加入I()这个函数，可以改变这种处理方式
# 相较于普通的data.frame函数，tibble/tribble函数处理这种情况会直观方便许多
tibble(
  x = list(1:3, 3:5),
  y = c('1, 2', '3, 4, 5')
)
tribble(
  ~x, ~y,
  1:3, '1, 2',
  3:5, '3, 4, 5'
)

# 在R的数据整理中，常常会用到nest()这个函数
# nest函数的作用很简单，就是按照我们上面的，生成列表列，从而做到数据嵌套的作用
df <- tibble(
  x1 = list('a,b,c', 'd,e,f,g')
)
df %>% mutate(x2 = stringr::str_split(x1, ',')) # 这里的x2，是一个有两个元素的列表，其实这就是数据嵌套后的形式
df %>% mutate(x2 = stringr::str_split(x1, ',')) %>% unnest()
# 这里unnest就把原来合在一起的x2元素拆开了
# 同时这里会有一个警告，建议指定我们需要处理的列
df %>% mutate(x2 = stringr::str_split(x1, ',')) %>% unnest(cols = c(x1, x2))
df_test <- df %>% mutate(x2 = stringr::str_split(x1, ',')) %>% unnest(cols = c(x1, x2)) %>%
  nest(.by = x1) # nest需要制定根据什么数据来进行数据合并，并且默认合并成df

# 这种列表列的另一个应用就是map函数族
sim <- tribble(
  ~f, ~params,
  'runif', list(min = -1, max = 1),
  'rnorm', list(sd = 5),
  'rpois', list(lambda = 10)
) # 构建了一个，函数与参数的信息矩阵
sim_result <- sim %>% mutate(sims = invoke_map(f, params, n = 10))
# 看一看sim_result的结果，可以发现我们非常清晰的构建出了函数/参数/结果的一个33矩阵
# 并且构建过程是高度自动化迭代的

# 面对大量数据的时候，我们常常会想知道这些数据的分位数
c(1:10) %>% quantile()
# 但是quantile这个函数搭配我们常用的summarize却有点难用
mtcars %>% group_by(cyl) %>% summarise(q = quantile(mpg))
mtcars %>% group_by(cyl) %>% summarise(n = n())
# 可以从上面的例子发现原因，summarise需要结果与group数是相同的，但是quantile返回的是一个任意长度的向量
# 可以将quantile用列表包装一下来解决这个问题
quantile_test <- mtcars %>% group_by(cyl) %>% summarise(q = list(quantile(mpg))) %>%
  unnest(cols = c(q))
# 不过这样的unnest结果可读性很差，可以进行一些改进
prob <- c(0.01, 0.25, 0.5, 0.75, 0.99)
quantile_test <- mtcars %>% group_by(cyl) %>% summarise(p = list(prob), q = list(quantile(mpg, prob))) %>%
  unnest(cols = c(q, p))
# 这样结果就具有了很好的可读性，并且要注意，unnest(cols = c(q, p))一定要指定相应的正确的列

# 有时候在处理比如基因等信息的时候，我们想要让一列是名字，一列是value
# 此时可以使用enframe函数，并且只要是list就可以
x <- list(a = 1:9, b = 3:31, c = 5:21)
(df <- enframe(x))
# 对于这种数据框，我们可以同时针对name/value来进行迭代
df %>% mutate(
  smry = map2_chr(
    name, value,
    ~ stringr::str_c(.x, ':', .y[1])
  )
) # 取出了每行的名字以及开头的数字


# 当我们拿到一个列表列的时候，特别是所含元素种类不同的时候，我们可以通过如下方式来展现它的特征
df <- tribble(
  ~x,
  LETTERS[1:5],
  1:3,
  runif(10)
)
df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
) # 这样就很清晰的展现了，这个列表列里面到底都有些啥

# 在上面提到了，unnest需要指定想要展开的列，如果我们想要展开的列，每一行元素数量不同，那显然是不行的
# 只有每一行元素数量相同，才有可能被展开
df1 <- tribble(
  ~x,~y,
  1:3,1:2
)
df1 %>% unnest(x, y) # x:y=2:3，这不能展开
df2 <- tribble(
  ~x,~y,
  1:3,1:3
)
df2 %>% unnest(x, y) # 这个能



