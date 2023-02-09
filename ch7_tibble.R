rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# tibble package
################################################################################


library(tidyverse)
# tibble��ʵ��data frame��һ�ֱ��壬�ṩ�˸�Ϊ�����ʹ�ã�������data frame����ת��
as_tibble(iris)
tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y 
) # �����е�������Python�е���������tibble�������Զ��ظ�����Ϊ1�����루Ҳ���ǵ������룩
# �����frame��˵��tibble���ܸ��٣����ܸı��������͡���������Ҳ���ܽ���������
tribble(
  ~x, ~y, ~z,
  'a', 1, 2,
  'b', 3, 4
) # ����tibble��ת����ʽ�����Զ��������ݽ��а������룬��һ���������ʽ�����Ǳ�����

###############tibble/data.frame�Ƚ�###############
# �Ľ��˴�ӡ�����
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = T)
)
nycflights13::flights %>% print(n = 30, width = Inf) # Inf��ʾ��ӡ������
# ȡ�Ӽ�
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df$x # ����ȡ����������
df[1] # ���Է��֣�����ȡ��������Ȼ��һ��tibble
df[[1]] # ����������һ������������
df %>% .$x # ��Ҫ�ڹܵ���ʹ�ã��Ǿ���.��Ϊռλ��

a <- df[[1]]
b <- enframe(a, name = 'test name', value = 'test value') # ���Խ�list��������ת��Ϊframe������������deframe
var <- 'test name'
# �����������浽һ�������У������ȡ����Ӧ�����ݿ�
b[var]
b[[var]]

# С��
df_test <- data.frame(abc = 1, xyz = 'a')
tb_test <- tibble(abc = 1, xyz = 'a')
df_test[, 'xyz']
df_test[, c('xyz', 'abc')]
df_test[1, 2]

