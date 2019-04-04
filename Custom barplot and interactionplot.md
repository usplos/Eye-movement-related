*本文档介绍自己编写的简便地做出单变量和双变量的条形图和交互作用图的函数*

## 单变量条形图

`BarplotUniV(df, IndeName, DeName,Fill = 'grey', Dot = F, alpha = 0.2, Color = 'black')`

### 参数介绍

`df` - 要处理的数据；

`IndeName` - 字符型，自变量的变量名；

`DeName` - 字符型，因变量的变量名；

`Fill` - 字符型，条形图填充颜色，默认为`grey`；

`Dot` - 逻辑型，是否画原始数据散点，默认不画；

`alpha` - 数值型，散点的透明度，默认为0.2；

`Color` - 字符型，三点的颜色，默认黑色；

### 示例

函数执行完毕，会输出图片，同时返回图片信息列表。

执行以下代码：
```
Demo = read_csv('https://raw.githubusercontent.com/usplos/self-programming/master/DemoCustomPlot.csv?token=AVnuGktnsOlSxKPsA78qjMwhiVaeJareks5cpCNJwA%3D%3D')
source('https://raw.githubusercontent.com/usplos/self-programming/master/CustomPlot.R')
p = BarplotUniV(df = Demo, DeName = 'FFD', IndeName = 'CondA', Dot = F)
```
输出图片：

<img src = 'https://github.com/usplos/self-programming/blob/master/BarplotuniV.png'>

## 双变量条形图

`BarplotBiV(df, IndeName, DeName,Fill = c('red','blue'), Dot = F, Alpha = 0.2, Color = 'black', Dodge = 0.5)`

### 参数介绍

`df` - 字符型，同单变量条形图；

`IndeName` - 字符型，两个自变量的变量名，用`c()`组合；

`DeName` - 字符型，同单变量条形图

`Fill` - 字符型，条形图填充颜色，需同时设置两种颜色，默认红蓝；

`Dot` - 逻辑型，是否画原始数据散点；

`Alpha` - 数值型，散点透明度，默认0.2；

`Color` - 字符型，散点颜色，默认黑色；

`Dodge` - 数值型，条形图位置参数，默认0.5；

### 示例

输出结果同单变量条形图

执行以下代码
```
p = BarplotBiV(Demo, c('CondA','CondB'), 'FFD', Fill = c('red','blue'), Dodge = 0.5)
```
输出图片：

<img src = 'https://github.com/usplos/self-programming/blob/master/BarplotBiV.png'>

## 交互作用图

 `InteractionplotBiV(df, IndeName, DeName,Dot = F, Alpha = 0.2, Color = c('red','blue'),Dodge = 0.5, PColor = 'black')`

### 参数介绍

`df` - 字符型，同上；

`IndeName` - 字符型，同双变量条形图；

`DeName` - 字符型，同双变量条形图；

`Dot` - 逻辑型，同双变量条形图；

`Alpha` - 数值型，同双变量条形图；

`Color` - 字符型，均值点的颜色，默认红蓝；

`Dodge` - 数值型，同双变量条形图；

`PColor` - 字符型，散点颜色，默认黑色

### 示例

输出结果同双变量条形图

执行以下代码
```
p = InteractionplotBiV(Demo, c('CondA','CondB'), 'FFD', Color = c('red','blue'), Dodge = 0.5)
```
输出图片

<img src = 'https://github.com/usplos/self-programming/blob/master/InteractionPlot.png'>

## 其他

这三个函数的优点除了适合初学者使用外，因依靠`ggplot`绘图，且能输出图形的列表信息，因此，用户可以在此基础上添加其他图形和调整其他参数。

