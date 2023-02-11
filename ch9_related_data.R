rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use dplyr to handle related data
################################################################################


library(tidyverse)
library(nycflights13)
# 对于一个庞大的数据集来说，某一个变量可能在不同的子集中出现，我们将其成为“键”
airlines
flights$carrier # 比如carrier这个变量同时在两个表格中出现了，就是键
# 对于键来说，可以同时是主键/外键
planes %>% count(tailnum) %>% filter(n>1) # 没有筛选到大于1的，证明是主键，变量可以代表所有观测值
weather %>% count(year) %>% filter(n>1) # 每年有很多个观测值，所以year并不是主键
# 在数据集中，也尝尝会出现找不到主键的情况，可以新生成代理键
flights %>% mutate(deference = arr_time-sched_arr_time) %>%
  count(year, month, day, tailnum, flight, carrier, deference) %>%
  filter(!is.na(tailnum), !is.na(deference), n>1)
# 通过合并连接，可以将两个数据表中相对应的变量放在同一个表中进行观察
(flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier))
airlines
flights2 %>% select(-origin, -dest) %>% left_join(airlines, by = 'carrier')
# 通过carrier这个变量进行一一匹配，加入了name这个对应的变量
# 用mutate也可以达到相似的结果
flights2 %>% select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])


##########连接的工作原理##########
x <- tribble(
  ~key, ~value_x,
  1, 'x1',
  2, 'x2',
  3, 'x3'
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2',
  4, 'y3'
) # 在这里可以理解为，1/2/3是键，而后面则是value，类似于Python中的键值对

# 内连接，仅仅匹配能够匹配到的变量值，容易丢失观测
(x %>% inner_join(y, by = 'key')) # 只有两行
# 外连接，对于匹配不到的变量值，至少保存一个表中的观测值
(x %>% left_join(y, by = 'key')) # 左连接，保留了x中匹配不上的
(x %>% right_join(y, by = 'key')) # 右连接，保留了y中匹配不上的
(x %>% full_join(y, by = 'key')) # 全连接，保留了xy中匹配不上的

# 而当数据表中存在键值对一对多的情况，则容易出现连接错误
x <- tribble(
  ~key, ~value_x,
  1, 'x1',
  2, 'x2',
  2, 'x3',
  1, 'x4'
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2'
)
x %>% left_join(y, by = 'key') # 并不能起到唯一观测的作用
x <- tribble(
  ~key, ~value_x,
  1, 'x1',
  2, 'x2',
  2, 'x3',
  3, 'x4'
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2',
  2, 'y3',
  3, 'y4'
)
x %>% left_join(y, by = 'key') # 两个表都存在键值对重复，则会出现全部的可能
flights %>% left_join(weather) # 默认使用所有的公共变量
flights2 %>% left_join(airports, by = c('dest' = 'faa'))
# 分别遍历两个表中的dest和faa，输出相等的结果


##########筛选连接##########
# 筛选连接会按照一定的标准对观测值进行筛选，而不是像上面的合并连接一样，单纯的把相同变量的进行合并
(top_dest <- flights %>% count(dest, sort = T) %>% head(10))
flights %>% filter(dest %in% top_dest$dest) # 手动构造一个筛选器
# 如果只有单个变量的话，那还是挺方便的，多个变量则建议使用半连接（筛选连接的方法）
# 半连接同样会对两个表进行合并，但是它并不添加新的列，只是会保留能够匹配上的观测值
flights %>% semi_join(top_dest)
# 半连接的逻辑是匹配上则留下，所以当存在重复键值对的时候，不会造成所有情况的出现，如下
x <- tribble(
  ~key, ~value_x,
  1, 'x1',
  2, 'x2',
  2, 'x3',
  3, 'x4'
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2',
  2, 'y3',
  3, 'y4'
)
x %>% semi_join(y) # x全部行都被保留了
# 而反连接anti_join()则是完全相反的了


##########集合操作##########
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)
intersect(df1, df2) # 取同时在两者之中的变量
union(df1, df2) # 返回x/y中的唯一观测，得到三行，注意理解“唯一”
setdiff(df1, df2) # 返回x中和y不一样的





