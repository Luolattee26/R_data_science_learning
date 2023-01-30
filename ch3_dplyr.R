rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# ʹ��dplyr��������ת������
################################################################################


if (require(dplyr) == F){
  BiocManager::install('dplyr')
}else{
  library(dplyr)
}
# dplyr�Ḳ��һЩ�������������ƣ������Ҫʹ�ã����������ĺ������֣�package::func
library(tidyverse)
library(nycflights13)
flights = nycflights13::flights
# ������filter������filter�������зǳ��򵥵Ĳ������ã�data+ɸѡ��׼
jan1_flight <- filter(flights, month == 1, day ==1)
(dec26_flight <- filter(flights, month == 12, day ==26))
# ��Ҫ��R��ͬʱ�������͸�ֵ�������ڸ�ֵ������������һ������
# ���ڼ�����ڼ����ʱ����õ������޾��ȣ����ԱȽϸ�����������ţ���ʱ����Ҫ����near����
near(sqrt(2)^2, 2)
near(1/49*49, 1)
# filter����Ĭ��ʹ�õĲ�����Ϸ�������&����ȻҲ����ʹ��������
filter(flights, month == 12 | month == 11)
# ���ſ��Բ���x %in% y����ʽ�����м�д
filter(flights, month %in% c(11,12))
# ��Ҫע����ǣ���R��������������NA����ô����Ľ�������Ҳ��NA
# ������ֵ���͵�ɸѡ��ʹ��between����Ҳ�Ǻܷ����
filter(flights, between(month, 11, 12)) # betweenȡ����һ��������
# arrange������filter���ƣ�������Ӧ�Ĳ����ṹ������ǰ��Ĳ����������ȼ�������
arrange(flights, desc(year), desc(month), day)
arrange(flights, desc(is.na(dep_time)))
# ��һ�����data�У�����ʹ��select��ѡ�����Ȥ���Ӽ�
select(flights, year, month)
select(flights, year:day) # ѡ���������е�
select(flights, -(year:day)) # ���з�ѡ
# �ڽ������ݴ�����ʱ�������ศ����������ѡ��
# starts_with('abc') | ends_with('abc')
# contains('abc')
# matches('(.)\\1') �����������ʽ
# num_range('x', 1:3) ƥ�京���ַ�Χ�ģ�����x1��x2��x3
# ����Ҫ�Ѽ��������ƶ������ݿ����ǰ�棬��everything()����������
select(flights, time_hour, air_time, everything())
# Ϊ�˷�ֹ�ظ�����������Ҳ���Դ���one_of()����һ��ʹ�ã����������������ѡ�����
# ��one_of('x', 'y')����ѡ��XY����������
select(flights,contains('TIME')) # ���Է��ָ���������Ĭ�Ϻ��Դ�Сд������ͨ��ignore.case�������ı�
# ��mutate���������Ծ������������µ���
flights_new <- select(flights, year:day, ends_with('delay'), 
                      distance, air_time)
mutate(flights_new, gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)
# ���ֻ���þɱ��������ݣ������뱣���ɱ����������ʹ��transmute
transmute(flights_new, gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)
# �ڴ����±���ʱ������ʹ��lag(),lead()����������
x <- c(1:10)
lead(x) # ǰ��
lag(x) # ����
x-lead(x) # �ƶ���ı仯ֵ
x != lead(x)
# ��R����ͬ�������Ⱥ�����������Python������
y <- c(1, 9, 3, NA, 4, 9)
min_rank(y) # ��ֵ��ͬ��Ĭ�ϻḳ��ͬ������
# ��Ҫ�����ݽ��л��ܴ���������summarize
summarise(flights, delay = mean(dep_delay, na.rm = T)) 
# �ڶ���������һ����ʽ����ʾ����������ܳ��Ľ����������ɶ
# summarizeͨ���Ǻ�group_byһ����
by_day <- group_by(flights, year, month, day)
# �������ԭ���ݲ���ɶ�仯�����ǻ�����������Ϣ���Ӷ�Ϊ����ĺ����ṩ
summarise(by_day, delay = mean(dep_delay, na.rm = T))