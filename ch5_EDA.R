rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# exploratory data analysis
################################################################################


library(tidyverse)
library(ggplot2)
library(dplyr)
# 在R中，分类变量通常会使用字符串或者因子的形式来进行储存
ggplot(data = diamonds, mapping = aes(x = cut)) + geom_bar()
diamonds %>% count(cut) # 利用count函数来进行分类变量的统计
diamonds %>% count(cut_width(carat, 0.5)) # 对连续变量进行切割
# 想要在分类变量的图中添加新的变量（如另一个分类变量），推荐使用折线图
ggplot(data = diamonds, mapping = aes(x = carat, color = cut)) + 
  geom_freqpoly(binwidth = 0.1) # 设置连续变量的分割宽度
# 当有异常值出现的时候，我们很难从直方图上面发现
ggplot(data = diamonds, mapping = aes(x = y)) + geom_histogram(binwidth = 0.5) # 可以发现y的取值特别特别大
# 可以使用coord_cartesian()将坐标轴的某一段放大
ggplot(data = diamonds, mapping = aes(x = y)) + geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50)) # 放大y轴0到50
# 可以发现存在三个异常值
(unusual <- diamonds %>% filter(y<3 | y>20) %>% arrange(y))
# 对于数据集中的异常值，我们可以选择两种方法，丢掉或者替换
# 一般推荐使用替换，尽可能的保留其他数据
diamonds2 <- diamonds %>% mutate(y = ifelse(y<3 | y>20, NA, y)) # 用mutate生成新的y变量
# ifelse函数有三个参数，条件、为真时的值，为假时的值
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point() # 会提醒去除了NA
# 想要看出两个变量的变化关系，折线图和直方图显然不太适合
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500) # 可以发现，不同组之间存在数量的巨大差异，不适合比较
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_histogram(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_bar(mapping = aes(color = cut))
# 注意直方图柱状图折线图均只能有一个X或Y
# 想要改变，则可以让y轴不是简单的显示count，而是显示密度
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) + 
  geom_histogram(mapping = aes(color = cut), binwidth = 500) # 可以发现就算加了density，还是不太适合
# 以分类变量来体现连续变量的分布，最好的方法应该是箱线图
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
# 对于一些分类变量来说，不一定天生就具有排好的顺序，此时我们可以使用reorder()来进行排序
ggplot(data = mpg, mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
         geom_boxplot() # reorder三个参数，分别是排序对象、依据、对依据进行的操作
# 箱线图有一个问题，并不适合体积极其庞大的数据集
# 使用geom_lv()
if(!require(lvplot)){
  install.packages('lvplot')
}else{
  library(lvplot)
}
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_lv(varwidth = T, width.method = "height")
# 以下是violin/分面hist/着色freploy的对比
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin(stat = 'ydensity')
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) +
  geom_histogram(binwidth = 500) + facet_wrap(facets = ~cut)
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) +
  geom_freqpoly(binwidth = 500, mapping = aes(color = cut))
# 探究两个分类变量的变化关系，则可以使用geom_count()
ggplot(data = diamonds, mapping = aes(x = cut, y = color)) + geom_count()
# 或者也可以使用tile图
diamonds %>% count(cut, color) %>%
ggplot(mapping = aes(x = cut, y = color)) + 
  geom_tile(mapping = aes(fill = n)) # 注意geom_tile函数需要传入数值变量
# 对于两个连续变量来说，当数据量大的时候，绘制散点图容易出现重复绘制的情况
# 用geom_bin2d/geom_hex这两个函数进行分箱
if(!require(hexbin)){
  install.packages('hexbin')
}else{
  library(hexbin)
}
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_bin2d()
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_hex() # 只是性状不一样
# 同样的，也可以利用cut_width
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.05)))
# 可以发现，box中必须传入一个分类变量，如果这里去除group，则会变成以每0.05carat为x轴进行绘图
# 此处的group =其实是传入了一个美学参数，并不会改变已经设置好的x = carat，但是会让box知道如何进行分组
# 想要在箱线图中通过箱子体现数据量，可以通过varwidth=T或者cut_number()
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20))) # 每个箱子20个观察值











