# 线性混合模型中的模型遍历

混合线性模型中，往往随机因子的个数不唯一，固定因子的个数也不唯一，因此存在若干个模型。
本文介绍作者自编的函数，作用是根据随机因子和固定因子，遍历所有模型，并获取这些模型的`AIC` / `BIC`值，以及是否收敛的情况，
以帮助我们筛选最优模型。

## 自编函数介绍

在R中执行下面命令

```
source('https://raw.githubusercontent.com/usplos/Eye-movement-related/master/LmmCode.R')
```

生成以下函数：
```
formula_generate(DV, IV, CLuster) # 生成formula，储存为csv文件，并将formula返回
LMMRun_Once(df, Formula, Family = NULL) # 单个模型的建立，返回其AIC，BIC，以及是否收敛的情况
LMMRun_Parallel(df, DV=NULL, IV=NULL, Cluster=NULL, Ifrun = F,
                   Manual = F, Manualcodefilename = NULL, Ncore = 4, Family = NULL) # 多线程建模，并返回这些模型的AIC，BIC，和是否收敛的情况
```

参数如下：
```
df - 数据框；
DV - 字符型，数据框中因变量的名称；
IV - 字符型，数据框中自变量的名称；
Formula - 字符型，建模公式，格式同lmer()函数中的公式格式；
Family - 字符型，因变量分布族，默认为NULL，即正态分布，其他分布可自行指定（广义模型）；
Ifrun - 逻辑型，是否运行模型，默认为F；
Manual - 逻辑型，是否从已有的formula中建模，默认为F；
Manualcodefilename - 字符型，已有的formula所在的文件名， 默认为NULL；
Ncore - 数值型，线程的个数，默认4线程；
```

根据模型的 `AIC` / `BIC` 以及是否收敛的情况来选取最优的模型。

## 示例

* 仅生成formula，并储存为YFormula.csv文件
```
Formulas = formula_generate(DV = 'Y',IV = c('A','B'), Cluster = c('Sub','Item'))
```

* 运行单个模型
```
ModelInfo = LMMRun_Once(df = DF, Formula = Formulas[1], Family = NULL)
```

* 多线程运行
```
ModelsInfo = LMMRun_Parallel(df = df, DV = 'Y', IV = c('A', 'B'), Cluster = c('Sub', 'Item'), Ifrun = T)
```

* 从已有formula运行
```
ModelsInfo = LMMRun_Parallel(df = df, Ifrun = T, Manual = T, Manualcodefilename = 'YFormula.csv')
```

* 8线程运行
```
ModelsInfo = LMMRun_Parallel(df = df, Ifrun = T, Manual = T, Manualcodefilename = 'YFormula.csv', Ncore = 8)
```

## 说明

* 仅适用于两个或三个固定因子的情况

* 默认适用于于两个随机因子的情况，如果只有一个随机因子，可以从生成的formulas中修改；
