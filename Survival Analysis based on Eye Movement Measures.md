# 眼动数据的生存分析

*可以对眼动的时间指标进行生存分析，比如首次注视时间，凝视时间等*

## 前期准备
请将[`SV.R`](https://raw.githubusercontent.com/usplos/self-programming/master/SV.R)文件内的全部代码拷贝到`RStudio`中执行，
以创建SV函数.

将数据（这里以首次注视时间为例）整理成[`DemoDataSV.csv`](https://raw.githubusercontent.com/usplos/self-programming/master/DemoDataSV.csv)
的样式。

## 参数
SV()中的参数如下：
* `Data` - 字符型，要处理的数据的文件名，比如`‘DemoDataSV.csv’`；
* `bootstrapNumber` - 整数型，重抽样的次数，默认10000；
* `perbinMax` - 整数型，bin的最大值，默认600；
* `perbinMin` - 整数型，bin的最小值，默认0；
* `basline` - 字符型，要比较的基线条件的名字；
* `Ylab` - 字符型，生存分析图的纵坐标标签；
* `Xlab` - 字符型，生存分析图的横坐标标签；

## 示例
以[`DemoDataSV.csv`](https://raw.githubusercontent.com/usplos/self-programming/master/DemoDataSV.csv)为例，
执行如下命令：

```
SV(Data = 'DemoDataSV.csv', bootstrapNumber = 200, perbinMin = 1, perbinMax = 600, baseline = 'C1',
    Xlab = 'First FIxation Duration (ms)', Ylab = 'Survival')
```

## 注意
* 仅适用于两条件的比较
* 条件的值的类型为字符型
* 需要安装以下数据包，虽然此函数会检查是否含有这些包（如果没有会先下载），但考虑到RStudio程序本身的问题，可事先安装好
```
install.packages('dplyr')
install.packages('tidyr')
install.packages('purrr')
install.packages('readr')
install.packages('ggplot2')
install.packages('tibble')
```
