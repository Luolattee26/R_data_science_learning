rm(list = ls())
setwd('C:/Users/Administrator/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use modelr to build complex models(not linear)
################################################################################


library(tidyverse)
library(modelr)
library(lubridate)
library(nycflights13)
options(na.action = na.warn)


# 在前面的数据中我们会发现，质量差的钻石反而具有更高的价格，我们是通过可视化来发现这个结论的，但是我们现在想要用数学语言来描述这个结论
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()
# 在钻石中，通常品质差的钻石会具有更大的重量，而重量是最能影响价格的一个因素
ggplot(diamonds, aes(carat, price)) + geom_hex(bins = 50) # 可以发现存在一个类似对数的关系
diamonds2 <- diamonds %>% filter(carat <= 2.5) %>% mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) + geom_hex(bins = 50)
# log处理后，发现了近似的线性关系
# 在构建模型的时候，我们常常使用log处理，因为它能让复杂的非线性关系转化为线性的
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
# 将数据还原，并且把预测值与真值进行比较
grid <- diamonds2 %>% data_grid(carat = seq_range(carat, 20)) %>% mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, 'lprice') %>% mutate(price = 2 ^ lprice)
ggplot(diamonds2, aes(carat, price)) + geom_hex(bins = 50) + geom_line(data = grid, color = 'green', size = 3)
# 可以看到，我们的模型基本上是正确的，但是肉眼可见的存在很大的误差，因为我们只考虑了carat这一个因素
# 看一下残差的情况
diamonds2 <- diamonds2 %>% add_residuals(mod_diamond, 'lresid')
ggplot(diamonds2, aes(carat, lresid)) + geom_hex(bins = 50)
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()
# 此时可以发现，将价格替换为预测的残差，品质与预测残差是呈现正相关的
# 这其实很好理解，我们的模型只考虑了重量与价格的关系，而当品质高的时候，颜色成色等同样会极大的影响钻石的价格


# 接下来，我们将其他的因素也考虑进去
mod_diamond2 <- lm(lprice ~ lcarat + cut + color + clarity, data = diamonds2)
# 因为这里涉及了多个因变量，所以单个变量对预测影响的可视化就变得有些麻烦了
(grid <- diamonds2 %>% data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2))
# 通俗的理解，加入.model参数后，对于我们想要分离出来的变量，其他变量data_grid会选择一个“typical value”来进行填充
# 比如这里的lcarat/color/clarity，如果是连续变量，则会取中值
ggplot(grid, aes(cut, pred)) + geom_point()


# 用同样的数据挖掘方法，来探究一下另一个飞机数据集
(daily <- flights %>% mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>% summarise(n = n())) # 得到每天起飞的航班数量
ggplot(daily, aes(date, n)) + geom_line()
# 如果直接这样看的话，很难挖掘出什么信息，因为这种数据存在非常强的周期效应
# 放在一个周期内，观察数据
(daily <- daily %>% mutate(wday = wday(date, label = T)))
ggplot(daily, aes(wday, n)) + geom_boxplot()
# 可以发现，星期几对飞机出发数量确实有影响，并且具有一定的规律
# 接下来使用大家最喜欢的线性模型
mod <- lm(n ~ wday, data = daily)
(grid <- daily %>% data_grid(wday) %>% add_predictions(mod, 'n'))
ggplot(daily, aes(wday, n)) + geom_boxplot() + geom_point(data = grid, color = 'red', size = 4)
# 计算残差，看预测效果
daily <- daily %>% add_residuals(mod)
ggplot(daily, aes(date, resid)) + geom_ref_line(h = 0, colour = 'red') + geom_line()
# 可以发现，从差不多六月开始，我们的模型就出现了很大的偏差值
ggplot(daily, aes(date, resid, color = wday)) + geom_ref_line(h = 0) + geom_line()
# 分日子来看，我们的模型主要是没有很好的预测周六的出发数量，夏季的实际数量多，而秋季的更少
# 整年看下来，似乎存在一种平滑的趋势，可以用smooth来可视化
ggplot(daily, aes(date, resid, color = wday)) + geom_ref_line(h = 0) + geom_line() +
  geom_smooth(se = F, span = 0.2)


# 上面我们提到，星期六似乎存在一些季节效应，使得我们的预测并不是很准确
# 先将星期六的数据单独提出来看看
daily %>% filter(wday == '周六') %>% ggplot(aes(date, n)) + 
  geom_point() + geom_line() + scale_x_date(
    NULL,
    date_breaks = '1 month',
    date_labels = '%b'
  ) # 可以发现，周六的航班出发数量存在十分明显的季节性波动
# 我们尝试创造一个“term”变量，来描述这种波动情况
term <- function(date){
  cut(date,
      breaks = ymd(2130101, 20130605, 20130825, 20140101),
      labels = c('spring', 'summer', 'fall'))
}
daily <- daily %>% mutate(term = term(date))
daily %>% filter(wday == '周六') %>% ggplot(aes(date, n, color = term)) + 
  geom_point() + geom_line() + scale_x_date(
    NULL,
    date_breaks = '1 month',
    date_labels = '%b'
  )
daily %>% ggplot(aes(wday, n, color = term)) + geom_boxplot() # 加入term分组之后，可以发现确实不同季节存在影响

# 拟合新的模型
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)
daily %>% gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) + geom_line()
# 可以发现，加入了term项之后貌似有改进，但是作用十分有限
# 覆盖回到原始数据，就能发现问题在哪
grid <- daily %>% data_grid(wday, term) %>% add_predictions(mod2, 'n')
ggplot(daily, aes(wday, n)) + geom_boxplot() + 
  geom_point(data = grid, color = 'red') + facet_wrap(~ term)
# 对于模型来说，考虑的往往是平均效应，但是在本数据中存在大量的离群值，这些离群值会很大的影响模型的预测效果
# 使用rlm可以一定程度上解决离群值带来的影响
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily %>% add_residuals(mod3, 'resid') %>% ggplot(aes(date, resid)) + geom_ref_line(h = 0, colour = 'green') +
  geom_line() # 可以发现，除了部分离群值之外，其他的回归效果都有了加强

# 当然，上面这种wday的转换方法，完全是可以写成一个函数，在构建模型的过程中直接使用这个函数就好了



