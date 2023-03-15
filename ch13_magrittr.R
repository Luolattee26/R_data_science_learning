rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use magrittr to construct pipe
################################################################################


library(magrittr)

########## %>% ����ԭ��##########
my_pipe <- function(.){
  . <- hop(., through = forest)
  . <- scoop(., up = filed_mice)
  bop(., on = head)
  } # ��ʵpipe����������һ�����ƵĴ��룬ʹ��.�������������
# ͨ��ԭ�����Է��֣�ĳЩ���������ʺ�ʹ��pipe������
# ʹ�õ�ǰ�����ĺ���
assign('x', 10) # �ڵ�ǰ������xΪ���ƴ���һ������x
x
'x' %>% assign(100)
x # ���Է�����Ȼ��10
# ��Ҫ�����������ǿ���ʹ����ʽָ�������ķ���
env <- environment()
'x' %>% assign(100, envir = env)
x
# ʹ�ö�����ֵģʽ�ĺ�����ֻ���ں��������õ�ʱ��������ֺ����Ĳ���
tryCatch(stop('!'), error = function(e)'An error') # �⺯���е���Python try/except
stop('!') %>% tryCatch(error = function(e)'An error')


##########magrittr��������##########
# %T>%���÷���%>%���ƣ����Ƿ��ص������ֵ
rnorm(100) %>% matrix(ncol = 2) %>% plot() %>% str() # ���Կ���û���ض�����
rnorm(100) %>% matrix(ncol = 2) %T>% plot() %>% str() # �����˾���
# %$%�����ʹ�õĺ�����֧�ֽ����ݿ���Ϊ�������룬ֻ֧����������������������ʹ�����
mtcars %$% cor(disp, mpg)


