rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# exploratory data analysis
################################################################################


library(tidyverse)
library(ggplot2)
library(dplyr)
# ��R�У��������ͨ����ʹ���ַ����������ӵ���ʽ�����д���
ggplot(data = diamonds, mapping = aes(x = cut)) + geom_bar()
diamonds %>% count(cut) # ����count���������з��������ͳ��
diamonds %>% count(cut_width(carat, 0.5)) # ���������������и�
# ��Ҫ�ڷ��������ͼ�������µı���������һ��������������Ƽ�ʹ������ͼ
ggplot(data = diamonds, mapping = aes(x = carat, color = cut)) + 
  geom_freqpoly(binwidth = 0.1) # �������������ķָ����
# �����쳣ֵ���ֵ�ʱ�����Ǻ��Ѵ�ֱ��ͼ���淢��
ggplot(data = diamonds, mapping = aes(x = y)) + geom_histogram(binwidth = 0.5) # ���Է���y��ȡֵ�ر��ر��
# ����ʹ��coord_cartesian()���������ĳһ�ηŴ�
ggplot(data = diamonds, mapping = aes(x = y)) + geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50)) # �Ŵ�y��0��50
# ���Է��ִ��������쳣ֵ
(unusual <- diamonds %>% filter(y<3 | y>20) %>% arrange(y))
# �������ݼ��е��쳣ֵ�����ǿ���ѡ�����ַ��������������滻
# һ���Ƽ�ʹ���滻�������ܵı�����������
diamonds2 <- diamonds %>% mutate(y = ifelse(y<3 | y>20, NA, y)) # ��mutate�����µ�y����
# ifelse����������������������Ϊ��ʱ��ֵ��Ϊ��ʱ��ֵ
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point() # ������ȥ����NA
# ��Ҫ�������������ı仯��ϵ������ͼ��ֱ��ͼ��Ȼ��̫�ʺ�
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500) # ���Է��֣���ͬ��֮����������ľ޴���죬���ʺϱȽ�
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_histogram(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_bar(mapping = aes(color = cut))
# ע��ֱ��ͼ��״ͼ����ͼ��ֻ����һ��X��Y
# ��Ҫ�ı䣬�������y�᲻�Ǽ򵥵���ʾcount��������ʾ�ܶ�
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) + 
  geom_histogram(mapping = aes(color = cut), binwidth = 500) # ���Է��־������density�����ǲ�̫�ʺ�
# �Է���������������������ķֲ�����õķ���Ӧ��������ͼ
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
# ����һЩ���������˵����һ�������;����źõ�˳�򣬴�ʱ���ǿ���ʹ��reorder()����������
ggplot(data = mpg, mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
         geom_boxplot() # reorder�����������ֱ�������������ݡ������ݽ��еĲ���
# ����ͼ��һ�����⣬�����ʺ���������Ӵ�����ݼ�
# ʹ��geom_lv()
if(!require(lvplot)){
  install.packages('lvplot')
}else{
  library(lvplot)
}
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_lv(varwidth = T, width.method = "height")
# ������violin/����hist/��ɫfreploy�ĶԱ�
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin(stat = 'ydensity')
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) +
  geom_histogram(binwidth = 500) + facet_wrap(facets = ~cut)
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) +
  geom_freqpoly(binwidth = 500, mapping = aes(color = cut))
# ̽��������������ı仯��ϵ�������ʹ��geom_count()
ggplot(data = diamonds, mapping = aes(x = cut, y = color)) + geom_count()
# ����Ҳ����ʹ��tileͼ
diamonds %>% count(cut, color) %>%
ggplot(mapping = aes(x = cut, y = color)) + 
  geom_tile(mapping = aes(fill = n)) # ע��geom_tile������Ҫ������ֵ����
# ������������������˵�������������ʱ�򣬻���ɢ��ͼ���׳����ظ����Ƶ����
# ��geom_bin2d/geom_hex�������������з���
if(!require(hexbin)){
  install.packages('hexbin')
}else{
  library(hexbin)
}
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_bin2d()
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_hex() # ֻ����״��һ��
# ͬ���ģ�Ҳ��������cut_width
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.05)))
# ���Է��֣�box�б��봫��һ������������������ȥ��group���������ÿ0.05caratΪx����л�ͼ
# �˴���group =��ʵ�Ǵ�����һ����ѧ������������ı��Ѿ����úõ�x = carat�����ǻ���box֪����ν��з���
# ��Ҫ������ͼ��ͨ����������������������ͨ��varwidth=T����cut_number()
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20))) # ÿ������20���۲�ֵ










