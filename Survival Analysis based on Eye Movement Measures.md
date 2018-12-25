# 眼动数据的生存分析

*下面介绍的函数可以对眼动的时间指标进行生存分析，比如首次注视时间，凝视时间等，对反应时等数据也可以*

## 前期准备
执行下面的命令，创建`SV()`函数
```
library(RCurl)
eval(parse(text = getURL('https://raw.githubusercontent.com/usplos/self-programming/master/SV.R')))
```

将数据（这里以首次注视时间为例）整理成[`DemoDataSV.csv`](https://raw.githubusercontent.com/usplos/self-programming/master/DemoDataSV.csv)
的样式，第一列为被试的编号，第二列为条件的编号，第三列为具体的值。**注意：三列的列名可随意，但是相对位置要固定，即第一列必须为被试，第二列为条件**

## 参数
`SV( )`中的参数如下：
* `Data` - 字符型，要处理的数据的文件名，比如`‘DemoDataSV.csv’`；
* `bootstrapNumber` - 整数型，重抽样的次数，默认10000；
* `perbinMax` - 整数型，bin的最大值，默认600；
* `perbinMin` - 整数型，bin的最小值，默认0；
* `basline` - 字符型，要比较的基线条件的编号；
* `Ylab` - 字符型，生存分析图的纵坐标标签，默认'DV'；
* `Xlab` - 字符型，生存分析图的横坐标标签，默认'IV'；
* `xp` - 数值型，生存分析图标记时间点分离点的x坐标，默认为`perbinMax-(perbinMax-perbinMin)/5`；
* `Cex` - 数值型，一般介于0 - 1，生存分析图图例字体大小, 默认0.8；
* `Bty` - 字符型，是否画图例的边框，取值为`'o'`或`'n'`，默认`'n'`
* `Width` - 数值型，图片的宽度，默认680；
* `Height` - 数值型，图片的高度，默认370

## 示例
以[`DemoDataSV.csv`](https://raw.githubusercontent.com/usplos/self-programming/master/DemoDataSV.csv)为例，
执行如下命令：

```
SV(Data = 'DemoDataSV.csv', bootstrapNumber = 200, perbinMin = 1, perbinMax = 600, baseline = 'C1',
    Xlab = 'First FIxation Duration (ms)', Ylab = 'Survival')
```

### 如果没有发现分离点

会输出以下结果：
```
Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

Want to understand how all the pieces fit together? Buy the ggplot2 book:
http://ggplot2.org/book/
Parsed with column specification:
cols(
  subj = col_character(),
  cond = col_character(),
  DV = col_integer()
)
S1  is done
S10  is done
C1  is done
###############################################################
S1  is done
S10  is done
C3  is done
###############################################################
There is no seperation.
Survival analysis is finished
 ################# ################# #################
```

### 如果发现分离点

会输出以下结果：
```
...
###############################################################
The first time point is  275  ms.
Survival analysis is finished
```
同时会输出以下的**生存分析图**：

![生存分析图](https://github.com/usplos/self-programming/blob/master/SurvivalPlot.png)

还会输出以下两个文件：`DemoDataSV_C1C3.csv`，`DemoDataSV_TimePoing.csv`。文件名的命名规则分别是

`<原始数据文件名前缀><_><条件1><条件2><.csv>`

`<原始数据文件名前缀><_><TimePoint><.csv>`

前者储存绘图所用的两个条件的数据，后者储存分离点的信息。因为有时候条件的名字太长，绘图不美观，
因此输出了原始数据，用户可根据具体的数据绘图。




## 注意
* 耗时较长，如果抽样次数为10000(一般为10000次)，perbin的范围设为0到600，一个被试的一个条件的数据大概需要2分钟，如果有20名被试，大概需要80分钟。如果要处理的数据过多，建议用`mapply()`这样的泛函数或循环，晚上休息时来做...；
* 建议先设置较少的抽样次数进行测试，根据输出的图像调整`xp`和`Cex`参数；
* 仅适用于两条件的比较；
* 条件的值的类型为字符型；
* 需要安装以下数据包，虽然此函数会检查是否含有这些包（如果没有会先下载），但考虑到RStudio程序本身的问题，可事先安装好：
```
install.packages('dplyr')
install.packages('tidyr')
install.packages('purrr')
install.packages('readr')
install.packages('ggplot2')
install.packages('tibble')
```
