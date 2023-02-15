rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use forcats to handle factor
################################################################################


library(tidyverse)
library(dplyr)
library(forcats)

# 因子在R中的意义非常简单，就是用来储存用于分类的变量
# 比如使用字符串形式来创建一个记录月份的变量
x1 <- c('Jan', 'Dec', 'May')
x2 <- c('Jam', 'Aprll', 'Dec') # 字符串看似可行，但是会发现，就算输入错了也不会有啥反馈
sort(x1) # 进行排序也完全无意义
# 想要创建因子，首先我们需要先对levels进行定义
month_levels <- c('Jan', 'Feb', 'Mar', 'April', 'May', 'Jun', 
                  'July', 'Oct', 'Sep', 'Nov', 'Dec') # 其实就是告诉R，我的因子靠什么标准进行分类
(y1 <- factor(x1, month_levels))
sort(y1) # 这样就会按照levels设定好的顺序进行排序
factor(x2, month_levels) # 输入错了，就会直接显示NA
# 如果不进行levels定义，则会将输入数据自动的作为levels
factor(x1) # 顺序按照字母的自动排序
factor(x1, unique(x1))
factor(x1) %>% forcats::fct_inorder() # 以上两种方法都可以按照输入顺序来排序

##########factor应用##########
gss_cat # 可以发现，其中许多变量都是因子数据
str(gss_cat$marital) # 6 levels factor
# 也有其他方法可以查看因子水平，并且进行计数
gss_cat %>% count(race)
ggplot2::ggplot(gss_cat, mapping = ggplot2::aes(x = race)) + ggplot2::geom_bar()
# 默认设置下，ggplot会删除count为0的level，可以强制显示
ggplot2::ggplot(gss_cat, mapping = ggplot2::aes(x = race)) + ggplot2::geom_bar() +
  ggplot2::scale_x_discrete(drop = F)

# 对levels进行修改
gss_cat %>% count(partyid)
gss_cat %>% mutate(partyid = fct_recode(partyid,
                       'Who fucking knows' = 'Strong republican')) %>% count(partyid)
# 要注意，fct_recode()和mutate()是合用的，并且格式为，修改后 = 修改前
# 如果将很多个levels同时修改成同样的新level，就起到了合并的作用
# 但是合并levels更好用的是fct_collapse()，可以直接通过向量形式输入
gss_cat %>% count(partyid)
gss_cat %>% mutate(partyid = fct_collapse(partyid,
                            other = c('No answer', 'Don\'t know'))) %>% count(partyid)







