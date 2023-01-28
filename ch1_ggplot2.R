rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


###############################################################################
#使用ggplot进行可视化的pipeline
###############################################################################


# 安装tidyverse
if (require(tidyverse) == F){
  BiocManager::install('tidyverse')
}else{
  print('tidyverse已安装')
  library(tidyverse)
}
# 引入mpg数据库，并且查看数据类型
mpg_data <- ggplot2::mpg
print(typeof(mpg_data)) # 可以发现是一个list
# 利用该数据库进行ggplot绘图
library(ggplot2)
ggplot(data = mpg_data) # 此处只会得到一张空图，作用是引用该数据库
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy))
# 此时可以得到一张散点图
# geom_point函数用于绘制散点图，aes函数用于设置数据的映射关系
# mapping参数和aes函数总是对应出现的
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy,
                                                   color = class))
# 对于mapping参数来说，作用就是把变量和图形映射起来，比如x，y，color，shape，alpha
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy),
                                                   color = 'green')
# 想要对图层（也就是散点图）进行整体的设置，那么需要在mapping中直接进行设置，而不能在aes中进行映射
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy,
                                                   color = displ < 5))
# 同时也可以利用aes函数的映射关系来对需要展示的数据进行一定的筛选
# 另一种在二维图形中添加额外变量的方法，是对图形进行分面，分成若干个子集
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(facets = ~class + cyl, nrow = 2) # 注意引入参数时的格式
# facet_wrap函数第一个参数就是分面的依据（可多个），同时也可以指定分面的矩阵形态
# 如果想要达到两个变量形成交叉的分面效果，则可以使用facet_grid函数
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(cyl ~ class)
# 使用不同的图形（图层进行绘制）
ggplot(data = mpg_data) + geom_smooth(mapping = aes(x = displ, y = hwy))
# smooth函数可以选择不同的method以及formula进行绘制
ggplot(data = mpg_data) + geom_smooth(mapping = aes(x = displ, y = hwy,
                                                    linetype = drv, color = drv)) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))
# 也有类似的添加变量的操作，或者是group参数
ggplot(data = mpg_data, mapping = aes(x = displ, y = hwy, group = drv, color = drv)) + 
  geom_point() + geom_smooth()
# 将相关信息写入最开始的数据库引用中，就可以很方便的直接将映射关系传递给后面的函数
# 对于不同的图层，当然也可以进行微调
ggplot(data = mpg_data, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg_data, class == 'subcompact'))
# 一些图形会对原有的数据进行自动的统计变换，产生新的数据
# 比如条形图，自动的产生了原来没有的count这个数据
ggplot(data = diamonds) + geom_bar(mapping = aes(x = price))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
# 其实上面的geom_smooth也是一样的，它会按照给定的模式进行拟合，输出曲线
# 查看stat参数的默认值，就可以看到该函数使用了哪种统计变换，比如bar的是stat_count()
ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))
# 通常来说，图形和几何变换函数是一一对应的关系，当然也可以自己进行指定统计变换
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))
# 使用color和fill这两个参数能够给条形图上色
# 如果fill参数被映射到另一个分组变量上面，那么条形图会发生堆叠，每一个色块表示两个变量之间的组合
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))
# 如果不想使用这种堆叠的效果，可以通过调整position参数来改变
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), alpha = 0.2,
                                   position = 'identity') # 将每一个对象直接显示，但是容易重叠，可以设置透明度解决
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity),
                                   position = 'fill') # 直接顶格，按比例显示
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity),
                                   position = 'dodge') # 拆开并列放置
# 在前面绘制散点图的时候，由于xy轴进行了四舍五入，所以有些点重合了，此时可以为点增加一个随机抖动来去重
ggplot(data = mpg_data) + geom_point(mapping = aes(x = displ, y = hwy), position = 'jitter')
# 在绘制箱线图的时候，常常会出现标签过长的情况，此时可以使用coord_flip()函数来解决
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()
# coord_polar()函数则可以带来极坐标
bar_plot <- ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))
bar_plot + coord_flip()
bar_plot + coord_polar()




