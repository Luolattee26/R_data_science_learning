rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use magrittr to construct pipe
################################################################################


library(magrittr)

########## %>% 工作原理##########
my_pipe <- function(.){
  . <- hop(., through = forest)
  . <- scoop(., up = filed_mice)
  bop(., on = head)
  } # 其实pipe就是运行了一段类似的代码，使用.来代替变量名称
# 通过原理可以发现，某些函数并不适合使用pipe来连接
# 使用当前环境的函数
assign('x', 10) # 在当前环境以x为名称创建一个变量x
x
'x' %>% assign(100)
x # 可以发现仍然是10
# 想要解决这个，我们可以使用显式指定环境的方法
env <- environment()
'x' %>% assign(100, envir = env)
x
# 使用惰性求值模式的函数，只会在函数被调用的时候计算这种函数的参数
tryCatch(stop('!'), error = function(e)'An error') # 这函数有点像Python try/except
stop('!') %>% tryCatch(error = function(e)'An error')


##########magrittr其他功能##########
# %T>%，用法与%>%类似，但是返回的是左侧值
rnorm(100) %>% matrix(ncol = 2) %>% plot() %>% str() # 可以看到没返回东西，
rnorm(100) %>% matrix(ncol = 2) %T>% plot() %>% str() # 返回了矩阵
# %$%，如果使用的函数不支持将数据框作为参数输入，只支持输入独立的向量，则可以使用这个
mtcars %$% cor(disp, mpg)



