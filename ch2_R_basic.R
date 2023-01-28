rm(list = ls())
setwd('c:/Users/Cal luo/Desktop/R_data_science_learning/')
getwd()


################################################################################
# ����ûɶ�õ�һ�£������浵
################################################################################


# ������Ϣѧ�����ر���һЩR�����������´��룬ֱ�����м��ɣ�
rm(list = ls())
# ���þ���
options()$repos
options()$BioC_mirror
#options(BioC_mirror="https://mirrors.ustc.edu.cn/bioc/")
options(BioC_mirror="http://mirrors.tuna.tsinghua.edu.cn/bioconductor/")
options("repos" = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
options()$repos
options()$BioC_mirror

# ����һ��
options()$repos
install.packages('WGCNA')
install.packages(c("FactoMineR", "factoextra"))
install.packages(c("ggplot2", "pheatmap","ggpubr"))
library("FactoMineR")
library("factoextra")

# ��������
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("KEGG.db",ask = F,update = F)
BiocManager::install(c("GSEABase","GSVA","clusterProfiler" ),ask = F,update = F)
BiocManager::install(c("GEOquery","limma","impute" ),ask = F,update = F)
BiocManager::install(c("org.Hs.eg.db","hgu133plus2.db" ),ask = F,update = F)

# ����������github�а�װ

# ���е�R�����ύ�ϴ���CRAN����Github����Ҫͨ��һ�����������а�װ
# R��װdevtools��
install.packages("devtools")
library(devtools)
# ��װgithub�ϵ�R�����跭ǽ���hosts��
devtools::install_github('lchiffon/REmap')
# ǰΪgithub���û�������Ϊ����

# ����--����R����
library(REmap)
library(GSEABase)
library(GSVA)
library(clusterProfiler)
library(ggplot2)
library(ggpubr)
library(hgu133plus2.db)
library(limma)
library(org.Hs.eg.db)
library(pheatmap)