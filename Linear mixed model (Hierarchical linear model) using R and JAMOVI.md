# 混合线性模型的实现（基于R和JAMOVI）
为什么要用混合线性模型：比如测量了不同收入水平的人群的收入和幸福感，但每个群体内收入水平是不同的，幸福感也不同，两者之间的关系也是不同的，
如果直接用一般线性模型，会造成错误的结论（见附加材料1），这个时候要考察的是可以推广到不同收入群体的收入和幸福感之间的关系
（即考察的关系**不仅可以应用于当前的收入群体，还可以应用到其他的群体**）。这时候需要用到混合线性模型（或者层次线性模型）。
## `R`
`R` 中混合线性模型要依靠`lmer`或者`lmerTest`数据包（强烈推荐后者，因为会输出显著性）
```
library(lmerTest)
```
### 基本表达式
`fit = lmer(data = , formula = DV ~ Fixed_Factor + (Random_intercept + Random_Slope | Random_Factor))`

`data` - 要处理的数据集；

`formula` - 表达式；

`DV` - 因变量；

`Fixed_Factor` - 固定因子，即考察的自变量；

`Random_intercept` - 随机截距，即认为不同群体的因变量的分布不同
（可以理解成有些人出生就在终点，而你是在起点......）；

`Random_Slope` - 随机斜率，即认为不同群体受固定因子的影响是不同的
（可以理解成别人花两个小时能赚10000元，而你只能挣个被试费......）；

`Random_Factor` - 随机因子；

### 数据整理形式
数据整理可参考[politeness](https://github.com/usplos/self-programming/blob/master/politeness_data.csv)

该数据整理若干被试（`subject`）的性别（`gender`），以及用不同的态度（`attitude`）在不同场合（`scenario`）下说话的音高（`frequency`）。
这是一个典型的被试内设计（7 \* 2设计）。

### 结果查看
以[politeness](https://github.com/usplos/self-programming/blob/master/politeness_data.csv)
数据为例，首先加载数据包并将`scenario`变量变为因子变量。

```
politeness = readr::read_csv('https://raw.githubusercontent.com/usplos/self-programming/master/politeness_data.csv')
politeness$scenario = factor(politeness$scenario)
library(lmerTest)
fit1 = lmer(frequency ~ scenario * attitude + (1|subject) + (1|gender), data = politeness)
```

建立模型，用`summary()`函数查看结果，
这里需要注意：**如果设置随机效应，模型可能无法收敛或者自由度溢出，这个时候需要调整或者取消随机效应**
```
fit1 = lmer(frequency ~ scenario * attitude + (1+attitude|subject) + (1+attitude|gender), data = politeness)
summary(fit1)
```

结果为

```
Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of  freedom
 [lmerMod]
Formula: frequency ~ scenario * attitude + (1 + attitude | subject) +  
    (1 + attitude | gender)
   Data: politeness

REML criterion at convergence: 680.1

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-1.65355 -0.68642 -0.03673  0.50259  2.85443 

Random effects:
 Groups   Name        Variance  Std.Dev. Corr 
 subject  (Intercept) 6.037e+02 24.5698       
          attitudepol 1.086e-02  0.1042  1.00 
 gender   (Intercept) 6.520e+03 80.7494       
          attitudepol 1.127e+02 10.6159  -1.00
 Residual             6.100e+02 24.6985       
Number of obs: 83, groups:  subject, 6; gender, 2

Fixed effects:
                      Estimate Std. Error      df t value Pr(>|t|)   
(Intercept)            180.767     58.843   1.050   3.072  0.19030   
scenario2               17.450     14.260  64.000   1.224  0.22554   
scenario3               46.667     14.260  64.000   3.273  0.00172 **
scenario4               44.833     14.260  64.000   3.144  0.00253 **
scenario5               16.800     14.260  64.000   1.178  0.24310   
scenario6                8.867     14.260  64.000   0.622  0.53628   
scenario7               18.133     14.260  64.000   1.272  0.20810   
attitudepol             -9.717     16.115   9.430  -0.603  0.56075   
scenario2:attitudepol   15.133     20.166  64.000   0.750  0.45575   
scenario3:attitudepol  -31.283     20.166  64.000  -1.551  0.12577   
scenario4:attitudepol   -4.650     20.166  64.000  -0.231  0.81837   
scenario5:attitudepol   -4.783     20.166  64.000  -0.237  0.81326   
scenario6:attitudepol  -14.703     20.699  64.030  -0.710  0.48008   
scenario7:attitudepol  -30.033     20.166  64.000  -1.489  0.14132   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

其中，随机效应的结果如下，可以看出确实对不同的被试或者性别来说，态度的影响是不同的，而且音高本身也是有差别的。
```
Random effects:
 Groups   Name        Variance  Std.Dev. Corr 
 subject  (Intercept) 6.037e+02 24.5698       
          attitudepol 1.086e-02  0.1042  1.00 
 gender   (Intercept) 6.520e+03 80.7494       
          attitudepol 1.127e+02 10.6159  -1.00
 Residual             6.100e+02 24.6985       
```



