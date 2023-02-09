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
# 除了传统的导入，还可以用readr_log()导入Apache格式的log文件，需要先安装webreadr包
# 在使用readr.csv()的时候，可以使用skip = n来跳过前n行读取，或者comment = #来忽略#开头的行
# 在R中，提供了默认的读取函数read.csv()，但是readr_csv()的性能更好
read_delim("x,y\n1,'a,b'", quote = '\'') 
# quote参数用来表示将字符串包裹起来的符号，在前面加\来起到区分的作用
read_csv("a,b\n1,3")
# parse_*()函数族可以接受一个字符向量，并且将其进行解析，返回解析结果（判断你输入的字符串是啥）
str(parse_integer(c('1', '2', '3', '*'), na = '*'))
# 在进行解析的时候，有一些方法上的参数需要设置，此时使用locale = locale()这个参数
str(parse_double(c('1,11', '2,22', '3,33', '*'), na = '*', locale = locale(decimal_mark = ",")))
# 比如这里，我告诉他使用【，】作为小数点符号
pharm <- c('Hengrui', 'Fosun', 'Beigene', 'Roche')
parse_factor(c('Hengrui', 'Fosun', 'Beigene', 'Roche', 'MSD'), levels = pharm)
# MSD不在我们设置的level中，所以会出现error
parse_factor(c('Hengrui', 'Fosun', 'Beigene', 'Roche', 'Fosun'), levels = pharm)

##########readr读取文件的机制##########
guess_parser("15:01") # 先对字符串类型进行猜测
parse_guess("15:01") # 利用之前的猜测进行解析
# 但是这种手段面对大量复杂数据常常会失效
challenge_old <- read_csv(readr_example("challenge.csv"))
problems(challenge_old) # 不知道为什么没有报错
# x前面全部是整数，后面是小数；y前面是NA，后面是日期
challenge_new <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_integer(),
    y = col_character()
  ) # 注意这里设置col_type所用到的格式
) # 但是这里就报错了，很奇怪
# 进行修改
(challenge_new <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)) # 可以发现，y列由于一开始全部是NA，被识别成了chr
(challenge_new <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)) 
# 默认的guess方式是1000行，有时候我们可以手动设置多一点，这样可以提高准确率
(challenge_newguess <- read_csv(
  readr_example('challenge.csv'),
  guess_max = 1001
)) # 可以看到，正确的读出了x/y的格式类型

##########进行更加高效的文件保存##########
# 当我们将数据保存为csv的时候，常常会导致数据类型的丢失，从而需要重新解析
# 在此引入RDs，这种R特有的二进制保存
write_rds(challenge_newguess, 'challenge.rds')
read_rds('challenge.rds') # 可以发现完美的保存了数据的类型信息
# 如果想将这种数据在不同语言共享，需要feather包
if(!require(feather)){
  BiocManager::install('feather')
}else{
  library(feather)
}
write_rds(challenge_newguess, 'challenge.feather')
read_rds('challenge.feather') 
# feather速度更快，但是RDs支持列表列





