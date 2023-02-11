rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use dplyr to handle related data
################################################################################


library(tidyverse)
library(nycflights13)
# ����һ���Ӵ�����ݼ���˵��ĳһ�����������ڲ�ͬ���Ӽ��г��֣����ǽ����Ϊ������
airlines
flights$carrier # ����carrier�������ͬʱ�����������г����ˣ����Ǽ�
# ���ڼ���˵������ͬʱ������/���
planes %>% count(tailnum) %>% filter(n>1) # û��ɸѡ������1�ģ�֤�����������������Դ������й۲�ֵ
weather %>% count(year) %>% filter(n>1) # ÿ���кܶ���۲�ֵ������year����������
# �����ݼ��У�Ҳ����������Ҳ�����������������������ɴ�����
flights %>% mutate(deference = arr_time-sched_arr_time) %>%
  count(year, month, day, tailnum, flight, carrier, deference) %>%
  filter(!is.na(tailnum), !is.na(deference), n>1)
# ͨ���ϲ����ӣ����Խ��������ݱ������Ӧ�ı�������ͬһ�����н��й۲�
(flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier))
airlines
flights2 %>% select(-origin, -dest) %>% left_join(airlines, by = 'carrier')
# ͨ��carrier�����������һһƥ�䣬������name�����Ӧ�ı���
# ��mutateҲ���Դﵽ���ƵĽ��
flights2 %>% select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])


##########���ӵĹ���ԭ��##########
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
) # �������������Ϊ��1/2/3�Ǽ�������������value��������Python�еļ�ֵ��

# �����ӣ�����ƥ���ܹ�ƥ�䵽�ı���ֵ�����׶�ʧ�۲�
(x %>% inner_join(y, by = 'key')) # ֻ������
# �����ӣ�����ƥ�䲻���ı���ֵ�����ٱ���һ�����еĹ۲�ֵ
(x %>% left_join(y, by = 'key')) # �����ӣ�������x��ƥ�䲻�ϵ�
(x %>% right_join(y, by = 'key')) # �����ӣ�������y��ƥ�䲻�ϵ�
(x %>% full_join(y, by = 'key')) # ȫ���ӣ�������xy��ƥ�䲻�ϵ�

# �������ݱ��д��ڼ�ֵ��һ�Զ������������׳������Ӵ���
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
x %>% left_join(y, by = 'key') # ��������Ψһ�۲������
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
x %>% left_join(y, by = 'key') # �����������ڼ�ֵ���ظ���������ȫ���Ŀ���
flights %>% left_join(weather) # Ĭ��ʹ�����еĹ�������
flights2 %>% left_join(airports, by = c('dest' = 'faa'))
# �ֱ�����������е�dest��faa�������ȵĽ��


##########ɸѡ����##########
# ɸѡ���ӻᰴ��һ���ı�׼�Թ۲�ֵ����ɸѡ��������������ĺϲ�����һ���������İ���ͬ�����Ľ��кϲ�
(top_dest <- flights %>% count(dest, sort = T) %>% head(10))
flights %>% filter(dest %in% top_dest$dest) # �ֶ�����һ��ɸѡ��
# ���ֻ�е��������Ļ����ǻ���ͦ����ģ������������ʹ�ð����ӣ�ɸѡ���ӵķ�����
# ������ͬ��������������кϲ������������������µ��У�ֻ�ǻᱣ���ܹ�ƥ���ϵĹ۲�ֵ
flights %>% semi_join(top_dest)
# �����ӵ��߼���ƥ���������£����Ե������ظ���ֵ�Ե�ʱ�򣬲��������������ĳ��֣�����
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
x %>% semi_join(y) # xȫ���ж���������
# ��������anti_join()������ȫ�෴����


##########���ϲ���##########
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
intersect(df1, df2) # ȡͬʱ������֮�еı���
union(df1, df2) # ����x/y�е�Ψһ�۲⣬�õ����У�ע�����⡰Ψһ��
setdiff(df1, df2) # ����x�к�y��һ����




