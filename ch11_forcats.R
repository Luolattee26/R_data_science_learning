rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use forcats to handle factor
################################################################################


library(tidyverse)
library(dplyr)
library(forcats)

# ������R�е�����ǳ��򵥣����������������ڷ���ı���
# ����ʹ���ַ�����ʽ������һ����¼�·ݵı���
x1 <- c('Jan', 'Dec', 'May')
x2 <- c('Jam', 'Aprll', 'Dec') # �ַ������ƿ��У����ǻᷢ�֣������������Ҳ������ɶ����
sort(x1) # ��������Ҳ��ȫ������
# ��Ҫ�������ӣ�����������Ҫ�ȶ�levels���ж���
month_levels <- c('Jan', 'Feb', 'Mar', 'April', 'May', 'Jun', 
                  'July', 'Oct', 'Sep', 'Nov', 'Dec') # ��ʵ���Ǹ���R���ҵ����ӿ�ʲô��׼���з���
(y1 <- factor(x1, month_levels))
sort(y1) # �����ͻᰴ��levels�趨�õ�˳���������
factor(x2, month_levels) # ������ˣ��ͻ�ֱ����ʾNA
# ���������levels���壬��Ὣ���������Զ�����Ϊlevels
factor(x1) # ˳������ĸ���Զ�����
factor(x1, unique(x1))
factor(x1) %>% forcats::fct_inorder() # �������ַ��������԰�������˳��������

##########factorӦ��##########
gss_cat # ���Է��֣������������������������
str(gss_cat$marital) # 6 levels factor
# Ҳ�������������Բ鿴����ˮƽ�����ҽ��м���
gss_cat %>% count(race)
ggplot2::ggplot(gss_cat, mapping = ggplot2::aes(x = race)) + ggplot2::geom_bar()
# Ĭ�������£�ggplot��ɾ��countΪ0��level������ǿ����ʾ
ggplot2::ggplot(gss_cat, mapping = ggplot2::aes(x = race)) + ggplot2::geom_bar() +
  ggplot2::scale_x_discrete(drop = F)

# ��levels�����޸�
gss_cat %>% count(partyid)
gss_cat %>% mutate(partyid = fct_recode(partyid,
                       'Who fucking knows' = 'Strong republican')) %>% count(partyid)
# Ҫע�⣬fct_recode()��mutate()�Ǻ��õģ����Ҹ�ʽΪ���޸ĺ� = �޸�ǰ
# ������ܶ��levelsͬʱ�޸ĳ�ͬ������level�������˺ϲ�������
# ���Ǻϲ�levels�����õ���fct_collapse()������ֱ��ͨ��������ʽ����
gss_cat %>% count(partyid)
gss_cat %>% mutate(partyid = fct_collapse(partyid,
                            other = c('No answer', 'Don\'t know'))) %>% count(partyid)






