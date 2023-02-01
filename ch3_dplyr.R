rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# 使用dplyr进行数据转换处理
################################################################################


if (require(dplyr) == F){
  BiocManager::install('dplyr')
}else{
  library(dplyr)
}
# dplyr会覆盖一些基础函数的名称，如果想要使用，就用完整的函数名字，package::func
library(tidyverse)
library(nycflights13)
flights = nycflights13::flights
# 首先是filter函数，filter函数具有非常简单的参数设置，data+筛选标准
jan1_flight <- filter(flights, month == 1, day ==1)
(dec26_flight <- filter(flights, month == 12, day ==26))
# 想要在R中同时输出结果和赋值，可以在赋值语句上整体加上一个括号
# 由于计算机在计算的时候采用的是有限精度，所以比较浮点数（如根号）的时候，需要加上near函数
near(sqrt(2)^2, 2)
near(1/49*49, 1)
# filter函数默认使用的参数结合方法是与&，当然也可以使用其他的
filter(flights, month == 12 | month == 11)
# 与门可以采用x %in% y的形式来进行简写
filter(flights, month %in% c(11,12))
# 需要注意的是，在R中如果计算包含了NA，那么计算的结果大概率也是NA
# 对于数值类型的筛选，使用between函数也是很方便的
filter(flights, between(month, 11, 12)) # between取的是一个闭区间
# arrange函数与filter类似，都是相应的参数结构，排在前面的参数排序优先级将更高
arrange(flights, desc(year), desc(month), day)
arrange(flights, desc(is.na(dep_time)))
# 在一个大的data中，可以使用select来选择感兴趣的子集
select(flights, year, month)
select(flights, year:day) # 选择两者其中的
select(flights, -(year:day)) # 进行反选
# 在进行数据处理的时候，有许多辅助函数可以选择
# starts_with('abc') | ends_with('abc')
# contains('abc')
# matches('(.)\\1') 这是正则表达式
# num_range('x', 1:3) 匹配含数字范围的，比如x1，x2，x3
# 当想要把几个变量移动到数据框的最前面，那everything()函数很有用
select(flights, time_hour, air_time, everything())
# 为了防止重复输入列名，也可以搭配one_of()函数一起使用，这个函数就是声明选择对象
# 如one_of('x', 'y')就是选择XY这两个变量
select(flights,contains('TIME')) # 可以发现辅助函数会默认忽略大小写，可以通过ignore.case参数来改变
# 用mutate函数，可以经过计算生成新的列
flights_new <- select(flights, year:day, ends_with('delay'), 
                      distance, air_time)
mutate(flights_new, gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)
# 如果只想用旧变量的数据，而不想保留旧变量，则可以使用transmute
transmute(flights_new, gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)
# 在创建新变量时，可以使用lag(),lead()这两个函数
x <- c(1:10)
lead(x) # 前移
lag(x) # 后移
x-lead(x) # 移动后的变化值
x != lead(x)
# 在R里面同样有排秩函数，类似于Python的排序
y <- c(1, 9, 3, NA, 4, 9)
min_rank(y) # 数值相同的默认会赋予同样的秩
# 想要对数据进行汇总处理，就是summarize
summarise(flights, delay = mean(dep_delay, na.rm = T)) 
# 第二个参数是一个公式，表示你最终想汇总出的结果的名字是啥
# summarize通常是和group_by一起用
by_day <- group_by(flights, year, month, day)
# 并不会对原数据产生啥变化，但是会增添分组信息，从而为后面的函数提供
summarise(by_day, delay = mean(dep_delay, na.rm = T))
# 像是在ggplot2里面使用+一样，对于dplyr也可以使用类似的方法
# 同时也可以使用“管道”来直接传递参数，从而避免写中间变量
# 管道符号，在阅读的时候，可以直接理解为then
(delays <- flights %>% group_by(dest) %>%
  summarise(count = n(),
            dist = mean(distance, na.rm = T),
            delay = mean(arr_delay, na.rm = T)) %>%
  filter(count > 20, dest != 'NHL'))
# 可以发现，使用了管道符号之后，后面的函数通通都没有data这个参数了，管道会自动的将前面的数据传入
# 在使用聚合函数，如summarize时，我们常常需要加入n()/sum()函数来判断我们聚合的数量到底是多少
# 从概率论里面很容易知道，数字越大越好）
not_canceled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_canceled %>% 
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))
delays <- not_canceled %>% group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay))
library(ggplot2)
ggplot(data = delays, mapping = aes(x = delay)) + geom_freqpoly(binwidth = 10)
# 再画散点图看看
(delays <- not_canceled %>% group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay, na.rm = T), n = n()))
# 可以发现，n参数返回了对应分组的大小，这样我们就可以去除那些太小的组防止随机误差
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 0.1) # 随着n增大，delay趋近于0
# +/%>%这两个连接符号，是可以同时使用的
delays %>% filter(n > 25) %>% 
  ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 0.1) # %>%可以传给ggplot，但是+不能传给dplyr
# 在R中进行数据筛选的时候，也可以使用简单的逻辑表达式来筛选
not_canceled %>% group_by(year, month, day) %>% 
  summarise(avg_delay1 = mean(arr_delay),
            avg_delay2 = mean(arr_delay[arr_delay>0]))
# 还有很多摘要函数能够搭配summarize来使用
# mean()/median()
# sd()/IQR()/mad()
# min()/quantile(x,0.25)/max()
# first()/nth(x,2)/last()
# n_distinct() 找出唯一值
# 在和数值一起使用的时候，bool会自动转化为0和1








