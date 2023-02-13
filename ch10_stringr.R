rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# use stringr to handle string data
################################################################################


library(tidyverse)
library(stringr)
# ��R�У�ͨ��ʹ��\�������ַ�����ת�⣬����ת�����ͬʱ����ӡ����������ʹ��writelines()���鿴ԭ������
test1 <- '123321\'' 
test1
writeLines(test1) # û�б�ʾ�ַ�����˫����
# ����ʹ��str_c()���������ַ����������
str_c('x', 'y')
# ��ʱ�ַ����д���NA������������ֱ�Ӱ�NA���ַ�������ʽ��ӡ���������������
x <- c('abc', NA)
str_c('{', x, '}') # ���Է��֣�NAû�б��������ţ���ΪĬ����ΪNA��ȱʧ
str_c('{', str_replace_na(x), '}')
# str_c()����������������ģ������Զ�ѭ���̵��������ҽ�����ϣ��Ӷ��ö������ͳ���������ͬ������
str_c('123456', c('aaa', 'bbb', 'ccc'), '123456')
# ʹ��str_sub()���Ժܷ����ȡ�ַ����Ӽ���ͬʱҲ���Խ����ַ����������޸�
x <- c('AAaaa', 'BBbbb', 'CCccc')
str_sub(x, 1, 2)
str_sub(x, -3, -1)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x


##########�������ʽ##########
library(htmltools)
library(htmlwidgets) # ���ӻ�
x <- c('apple', 'banana', 'pear')
str_view(x, 'ea') # ƥ�䵽��x�еġ�ea��
str_view(x, '.a.') # ����ĺ�����ǣ�ƥ��һ��lenΪ3���ַ�����[2]Ϊa
# ��Ҫע����ǣ�����\���ַ������������ʽ�ж���ʾת�⣬���Ծ�����Ҫ\\����������
dot <- '\\.'
writeLines(dot) # \. ����ĵ�һ��\����ת������ã�ʣ�µ�\.��Ϊ�ַ�������
str_view(c('abc', 'a.c', 'bef'), 'a\\.c') # �ڶ��������е��������ʽ����һ��\��.ת�⣬�ڶ���\��\.ת��
x <- 'a\\b'
writeLines(x)
str_view(x, '\\\\')
# ʹ��ê�㣬�������������ʽ����һ��˳�����ƥ��
x <- c('apple', 'banana', 'pear')
str_view(x, '^a') # ��ͷ��ʼƥ��
str_view(x, 'a$') # ��β��ʼƥ��
x <- c('apple', 'apple INC', 'apple pie')
str_view(x, '^apple$') # ����ƥ��
test1 <- '$^$'
writeLines(test1)
str_view(test1, '\\$\\^\\$') # ���Է��֣���ÿһ����Ҫת����ַ�����Ҫ����ʹ��\\ת��
# ���������ʽ�У��ж�������ƥ��ģʽ������ǰ����ܹ���.
# \dƥ���������֣�\sƥ����һ�հ��ַ�
# [abc]ƥ��a��b��c��[^abc]ƥ��abc�����
# �߼�����Ҳͬ�����������������ʽ��
str_view(c('grey', 'gray', 'grwy'), 'gr(e|a|w)y')
# �������ʽ��ƥ�������Ҳ����ͨ���������������
x <- '1888 is the longest year in Roman numerals:MDCCCLXXXVIII'
str_view(x, 'CC?') # ?:0�λ�1�Σ�ע������������ǰ����Ǹ��ַ�
str_view(x, 'CC+') # +:1�λ���
str_view(x, 'CC*') # *:�����
str_view(x, 'C[LX]+') # ���Դ���������һ��ʹ��
str_view('i like the banana which had a tough bananananana', 'bana(na)+')
str_view(x, 'C{2}') # �����ض��Ĵ���
str_view(x, 'C{2,}')
str_view(x, 'C{2,3}')
# �������ʽ��ƥ���߼�ͨ����̰���ģ���ƥ�価���ܳ����ַ���������������?���Խ����Ϊ�����
str_view(x, 'C[LX]+?')
str_view(x, 'C{2,3}?')
# �����ʹ���˷�����������õļ���
str_view(fruit, '(..)\\1', match = T)
# ͨ��(..)���з��飬�������������ַ�������ʹ��\1���л���
# ע�⣬�˴���\\1�����߼��������\ת���߼�����ȫ��ͬ�ģ������˽�������ת��
str_view(c('AaAa111111', 'BbBb222222', 'CcCc333333'), 
         '(..)\\1(...)\\2', match = T) # ���ﹹ����2��3�������飬���ҷֱ���Ϊ1&2�Ž��л�������


##########�����������ʽ�������ִ���##########
x <- c('apple', 'banana', 'pear')
str_detect(x, 'e') # �����߼�ֵ
sum(str_detect(words, '^t'))
mean(str_detect(words, '[aeiou]$'))
# ���������ر��ӵ��������ʽ�������������߼�ֵ�����д���
no_vowels_1 <- !str_detect(words, '[aeiou]')
no_vowels_2 <- str_detect(words, '^[^aeiou]+$')
identical(no_vowels_1, no_vowels_2) # ���ҳ������˲�����Ԫ����ĸ�ĵ��ʣ�������Ȼ�߼��������

words[str_detect(words, 'x$')]
str_subset(words, 'x$') # ע���str_sub()������

df <- tibble(
  word = words,
  i = seq_along(word)
)
df %>% filter(str_detect(words, 'x$'))

x <- c('apple', 'banana', 'pear')
mean(str_count(x, 'a'))

# ����ƥ�����ֵ���ȡ
length(sentences)
head(sentences)
colors <- c('red', 'green', 'yellow', 'blue', 'purple')
(colors_match <- str_c(colors, collapse = '|')) # �ϳ�Ϊһ���ַ���
# ��ϸ�۲���Է��֣�colors_match����ַ������Ѿ���һ���������ʽ��
(has_color <- str_subset(sentences, colors_match))
(matched_color <- str_extract(has_color, colors_match)) # str_extract()ֻ����ȡ��һ��ƥ��
more <- sentences[str_count(sentences, colors_match)>1]
str_view_all(more, colors_match) # ���ƥ��
str_extract_all(more, colors_match, simplify = T) # ���ƥ�䣬�������÷��ؾ���

noun <- '(a|the) ([^ ]+)' # ��Ҫ�ҵ�a/the��������е��ʣ�ע���м�Ҫ���Ͽո�
(has_noun <- sentences %>% str_subset(noun) %>% head(10))
has_noun %>% str_extract(noun)
has_noun %>% str_match(noun) # ���ؾ��󣬺����б�ʾ������ƥ�����
# ���ǿ��Է��֣�������һЩ���ݴ�Ҳƥ�������
# Ҳ����ת����tibble�У�����tidyr���д���
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c('article', 'noun'), '(a|the) ([^ ]+)', 
    remove = F
  ) # ͨ���½����б�������ʽ��������ȡ

# ����ƥ�����ֵ��滻
x <- c('apple', 'banana', 'pear')
str_replace(x, '[aeiou]', '-')
str_replace_all(x, '[aeiou]', '-')
# ����һ��������ʽ������������ʵ�������滻
x <- c('1 one', '2 two')
str_replace_all(x, c('1' = 'one', '2' = 'two')) # ����һ��Ҫ��all
x <- '1888 is the longest year in Roman numerals:MDCCCLXXXVIII'
str_replace(x, '([^ ]+) ([^ ]+) ([^ ]+)', '\\1 \\3 \\2') # ���÷���ͻ������ã������˵���˳���滻

# �������ֵĲ�ִ���
sentences %>% head(5) %>% strsplit(' ') # ���Է��ַ��ص���һ���б�
# ����ǵ��ַ���֣�����Խ������´���
a <- 'a|b|c|d|e'
a %>% strsplit('\\|') %>% .[[1]]

# �ڴ����������ʽ��Ϊ������ʱ����ʵ������д���ǵ���regex()
a <- 'a|b|c|d|e'
a %>% strsplit(regex('\\|')) %>% .[[1]] # һЩƥ��ģʽ���������������Сд��Ҳ������regex()�н����趨
# ����һЩ�������õ�ƥ��ģʽ�����R_data_science


##########�������ʽ������Ӧ��##########
apropos('replace') # ����������
dir(pattern = '\\.R$') # ���뵱ǰĿ¼��������R�ļ�
# dir()�����г�ĳ��Ŀ¼����������ļ���pattern�����ɴ����������ʽ


# stringi����stringr�ĸ��Ӱ�


