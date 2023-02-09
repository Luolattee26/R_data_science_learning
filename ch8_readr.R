rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# usage of readr
################################################################################


library(tidyverse)
library(readr)
if(!require(webreadr)){
  BiocManager::install('webreadr')
}else{
  library(webreadr)
}
# ���˴�ͳ�ĵ��룬��������readr_log()����Apache��ʽ��log�ļ�����Ҫ�Ȱ�װwebreadr��
# ��ʹ��readr.csv()��ʱ�򣬿���ʹ��skip = n������ǰn�ж�ȡ������comment = #������#��ͷ����
# ��R�У��ṩ��Ĭ�ϵĶ�ȡ����read.csv()������readr_csv()�����ܸ���
read_delim("x,y\n1,'a,b'", quote = '\'') 
# quote����������ʾ���ַ������������ķ��ţ���ǰ���\�������ֵ�����
read_csv("a,b\n1,3")
# parse_*()��������Խ���һ���ַ����������ҽ�����н��������ؽ���������ж���������ַ�����ɶ��
str(parse_integer(c('1', '2', '3', '*'), na = '*'))
# �ڽ��н�����ʱ����һЩ�����ϵĲ�����Ҫ���ã���ʱʹ��locale = locale()�������
str(parse_double(c('1,11', '2,22', '3,33', '*'), na = '*', locale = locale(decimal_mark = ",")))
# ��������Ҹ�����ʹ�á�������ΪС�������
pharm <- c('Hengrui', 'Fosun', 'Beigene', 'Roche')
parse_factor(c('Hengrui', 'Fosun', 'Beigene', 'Roche', 'MSD'), levels = pharm)
# MSD�����������õ�level�У����Ի����error
parse_factor(c('Hengrui', 'Fosun', 'Beigene', 'Roche', 'Fosun'), levels = pharm)

##########readr��ȡ�ļ��Ļ���##########
guess_parser("15:01") # �ȶ��ַ������ͽ��в²�
parse_guess("15:01") # ����֮ǰ�Ĳ²���н���
# ���������ֶ���Դ����������ݳ�����ʧЧ
challenge_old <- read_csv(readr_example("challenge.csv"))
problems(challenge_old) # ��֪��Ϊʲôû�б���
# xǰ��ȫ����������������С����yǰ����NA������������
challenge_new <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_integer(),
    y = col_character()
  ) # ע����������col_type���õ��ĸ�ʽ
) # ��������ͱ����ˣ������
# �����޸�
(challenge_new <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)) # ���Է��֣�y������һ��ʼȫ����NA����ʶ�����chr
(challenge_new <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)) 
# Ĭ�ϵ�guess��ʽ��1000�У���ʱ�����ǿ����ֶ����ö�һ�㣬�����������׼ȷ��
(challenge_newguess <- read_csv(
  readr_example('challenge.csv'),
  guess_max = 1001
)) # ���Կ�������ȷ�Ķ�����x/y�ĸ�ʽ����

##########���и��Ӹ�Ч���ļ�����##########
# �����ǽ����ݱ���Ϊcsv��ʱ�򣬳����ᵼ���������͵Ķ�ʧ���Ӷ���Ҫ���½���
# �ڴ�����RDs������R���еĶ����Ʊ���
write_rds(challenge_newguess, 'challenge.rds')
read_rds('challenge.rds') # ���Է��������ı��������ݵ�������Ϣ
# ����뽫���������ڲ�ͬ���Թ�������Ҫfeather��
if(!require(feather)){
  BiocManager::install('feather')
}else{
  library(feather)
}
write_rds(challenge_newguess, 'challenge.feather')
read_rds('challenge.feather') 
# feather�ٶȸ��죬����RDs֧���б���




