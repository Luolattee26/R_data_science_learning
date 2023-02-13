rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use stringr to handle string data
################################################################################


library(tidyverse)
library(stringr)
# 在R中，通常使用\来表达字符串的转意，但是转意符会同时被打印出来，可以使用writelines()来查看原本内容
test1 <- '123321\'' 
test1
writeLines(test1) # 没有表示字符串的双引号
# 可以使用str_c()函数来对字符串进行组合
str_c('x', 'y')
# 有时字符串中存在NA，但是我们想直接把NA以字符串的形式打印出来，则可以这样
x <- c('abc', NA)
str_c('{', x, '}') # 可以发现，NA没有被加上括号，因为默认认为NA是缺失
str_c('{', str_replace_na(x), '}')
# str_c()这个函数是向量化的，它会自动循环短的向量并且进行组合，从而让短向量和长向量具有同样长度
str_c('123456', c('aaa', 'bbb', 'ccc'), '123456')
# 使用str_sub()可以很方便的取字符串子集，同时也可以进行字符串的批量修改
x <- c('AAaaa', 'BBbbb', 'CCccc')
str_sub(x, 1, 2)
str_sub(x, -3, -1)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x


##########正则表达式##########
library(htmltools)
library(htmlwidgets) # 可视化
x <- c('apple', 'banana', 'pear')
str_view(x, 'ea') # 匹配到了x中的‘ea’
str_view(x, '.a.') # 这里的含义就是，匹配一个len为3的字符串，[2]为a
# 需要注意的是，由于\在字符串与正则表达式中都表示转意，所以经常需要\\来进行输入
dot <- '\\.'
writeLines(dot) # \. 输入的第一个\起到了转意的作用，剩下的\.作为字符串本体
str_view(c('abc', 'a.c', 'bef'), 'a\\.c') # 第二个参数中的正则表达式，第一个\将.转意，第二个\将\.转意
x <- 'a\\b'
writeLines(x)
str_view(x, '\\\\')
# 使用锚点，可以让正则表达式按照一定顺序进行匹配
x <- c('apple', 'banana', 'pear')
str_view(x, '^a') # 从头开始匹配
str_view(x, 'a$') # 从尾开始匹配
x <- c('apple', 'apple INC', 'apple pie')
str_view(x, '^apple$') # 整段匹配
test1 <- '$^$'
writeLines(test1)
str_view(test1, '\\$\\^\\$') # 可以发现，对每一个需要转意的字符，都要单独使用\\转意
# 在正则表达式中，有多种特殊匹配模式，例如前面介绍过的.
# \d匹配任意数字，\s匹配任一空白字符
# [abc]匹配a或b或c，[^abc]匹配abc以外的
# 逻辑符号也同样可以用于正则表达式中
str_view(c('grey', 'gray', 'grwy'), 'gr(e|a|w)y')
# 正则表达式的匹配次数，也可以通过特殊符号来控制
x <- '1888 is the longest year in Roman numerals:MDCCCLXXXVIII'
str_view(x, 'CC?') # ?:0次或1次，注意是搜索符号前面的那个字符
str_view(x, 'CC+') # +:1次或多次
str_view(x, 'CC*') # *:任意次
str_view(x, 'C[LX]+') # 可以搭配其他的一起使用
str_view('i like the banana which had a tough bananananana', 'bana(na)+')
str_view(x, 'C{2}') # 设置特定的次数
str_view(x, 'C{2,}')
str_view(x, 'C{2,3}')
# 正则表达式的匹配逻辑通常是贪婪的，会匹配尽可能长的字符串，在最后面加上?可以将其改为懒惰的
str_view(x, 'C[LX]+?')
str_view(x, 'C{2,3}?')
# 在这里，使用了分组与回溯引用的技巧
str_view(fruit, '(..)\\1', match = T)
# 通过(..)进行分组，代表任意两个字符，而后使用\1进行回溯
# 注意，此处的\\1构造逻辑和上面的\转意逻辑是完全相同的，代表了进行两层转意
str_view(c('AaAa111111', 'BbBb222222', 'CcCc333333'), 
         '(..)\\1(...)\\2', match = T) # 这里构造了2和3两个分组，并且分别作为1&2号进行回溯引用


##########利用正则表达式进行文字处理##########
x <- c('apple', 'banana', 'pear')
str_detect(x, 'e') # 返回逻辑值
sum(str_detect(words, '^t'))
mean(str_detect(words, '[aeiou]$'))
# 尽量避免特别复杂的正则表达式，而尝试利用逻辑值来进行处理
no_vowels_1 <- !str_detect(words, '[aeiou]')
no_vowels_2 <- str_detect(words, '^[^aeiou]+$')
identical(no_vowels_1, no_vowels_2) # 都找出所有了不包含元音字母的单词，但是显然逻辑运算更简单

words[str_detect(words, 'x$')]
str_subset(words, 'x$') # 注意和str_sub()的区分

df <- tibble(
  word = words,
  i = seq_along(word)
)
df %>% filter(str_detect(words, 'x$'))

x <- c('apple', 'banana', 'pear')
mean(str_count(x, 'a'))

# 进行匹配文字的提取
length(sentences)
head(sentences)
colors <- c('red', 'green', 'yellow', 'blue', 'purple')
(colors_match <- str_c(colors, collapse = '|')) # 合成为一个字符串
# 仔细观察可以发现，colors_match这个字符串就已经是一个正则表达式了
(has_color <- str_subset(sentences, colors_match))
(matched_color <- str_extract(has_color, colors_match)) # str_extract()只会提取第一个匹配
more <- sentences[str_count(sentences, colors_match)>1]
str_view_all(more, colors_match) # 多个匹配
str_extract_all(more, colors_match, simplify = T) # 多个匹配，并且设置返回矩阵

noun <- '(a|the) ([^ ]+)' # 想要找到a/the后面的所有单词，注意中间要加上空格
(has_noun <- sentences %>% str_subset(noun) %>% head(10))
has_noun %>% str_extract(noun)
has_noun %>% str_match(noun) # 返回矩阵，后两列表示单独的匹配情况
# 但是可以发现，这样把一些形容词也匹配进来了
# 也可以转化到tibble中，利用tidyr进行处理
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c('article', 'noun'), '(a|the) ([^ ]+)', 
    remove = F
  ) # 通过新建两列变量的形式来进行提取

# 进行匹配文字的替换
x <- c('apple', 'banana', 'pear')
str_replace(x, '[aeiou]', '-')
str_replace_all(x, '[aeiou]', '-')
# 传入一个命名形式的向量，可以实现批量替换
x <- c('1 one', '2 two')
str_replace_all(x, c('1' = 'one', '2' = 'two')) # 这里一定要用all
x <- '1888 is the longest year in Roman numerals:MDCCCLXXXVIII'
str_replace(x, '([^ ]+) ([^ ]+) ([^ ]+)', '\\1 \\3 \\2') # 利用分组和回溯引用，做到了单词顺序替换

# 进行文字的拆分处理
sentences %>% head(5) %>% strsplit(' ') # 可以发现返回的是一个列表
# 如果是单字符拆分，则可以进行如下处理
a <- 'a|b|c|d|e'
a %>% strsplit('\\|') %>% .[[1]]

# 在传入正则表达式作为参数的时候，其实完整的写法是调用regex()
a <- 'a|b|c|d|e'
a %>% strsplit(regex('\\|')) %>% .[[1]] # 一些匹配模式的其他参数（如大小写）也可以在regex()中进行设定
# 还有一些更不常用的匹配模式，详见R_data_science


##########正则表达式的其他应用##########
apropos('replace') # 搜索函数名
dir(pattern = '\\.R$') # 传入当前目录下面所有R文件
# dir()可以列出某个目录下面的所有文件，pattern参数可传入正则表达式


# stringi就是stringr的复杂版



