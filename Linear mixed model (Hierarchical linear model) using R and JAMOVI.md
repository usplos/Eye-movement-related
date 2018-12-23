*文本档读者登录GitHub后可自行修改补充，修改时请注意使用[Markdown的语法](https://www.cnblogs.com/yabin/p/6366151.html)。*

# 混合线性模型的实现（基于R和JAMOVI）
为什么要用混合线性模型：比如测量了不同收入水平的人群的收入和幸福感，但每个群体内收入水平是不同的，幸福感也不同，两者之间的关系也是不同的，
如果直接用一般线性模型，会造成错误的结论（见[附录](https://github.com/usplos/self-programming/blob/master/%E9%99%84%E5%BD%95.pdf)），这个时候要考察的是可以推广到不同收入群体的收入和幸福感之间的关系
（即考察的关系**不仅可以应用于当前的收入群体，还可以应用到其他的群体**）。这时候需要用到混合线性模型（或者层次线性模型）。
## `R`
`R` 中混合线性模型要依靠`lme4`或者`lmerTest`数据包（强烈推荐后者，因为会输出显著性）
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
数据整理可参考`data1`：

<img src = 'https://github.com/usplos/self-programming/blob/master/data1.png'>


该数据收集了若干被试（`Sub`）在两种条件下（`CondA`，`CondB`）的首次注视时间（`FFD`）。 这是一个典型的被试内设计（2 × 2设计）。

### 结果查看
以`data1`为例，
首先将`CondA`和`CondB`设置为因子变量，加载`lmerTest`数据包。

```
data1$CondA = factor(data1$CondA)
data1$CondB = factor(data1$CondB)
library(lmerTest)
```

建立模型，用`summary()`函数查看结果， 这里需要注意：
* 如果自变量是群体（个体）间的设计，就不能添加随机斜率，这里的两个条件是被试内的，所以可以设置为随机斜率，而像年龄（每个被试只有一个确定的年龄）、性别（被试不可能既是男的又是女的）等变量不可以作为随机斜率；
* 如果设置随机效应，模型可能无法收敛或者自由度溢出(见 《随机斜率的取舍》部分)，这个时候需要调整或者取消随机效应；
* 一般只加`Sub`的斜率，`Item`只加随机截距，因为固定因子和因变量间的关系在不同项目间的差异是较小的，但是这种关系在不同被试间的差异是比较大的：

```
fit1 = lmer(data = data1, FFD ~ CondA * CondB + (1 + CondA*CondB | Sub) + (1|Item))
summary(fit1)
```

结果为

```
Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of freedom [lmerMod]
Formula: FFD ~ CondA * CondB + (1 + CondA * CondB | Sub) + (1 | Item)
   Data: data1

REML criterion at convergence: 2202

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.904 -0.637 -0.238  0.444  3.376 

Random effects:
 Groups   Name            Variance Std.Dev. Corr             
 Item     (Intercept)      1622     40.3                     
 Sub      (Intercept)      1467     38.3                     
          CondAA2           794     28.2    -1.00            
          CondBB2           649     25.5    -1.00  1.00      
          CondAA2:CondBB2   363     19.1     1.00 -1.00 -1.00
 Residual                 10237    101.2                     
Number of obs: 183, groups:  Item, 64; Sub, 3

Fixed effects:
                Estimate Std. Error     df t value Pr(>|t|)   
(Intercept)       268.08      27.69   2.10    9.68   0.0089 **
CondAA2            17.73      27.35   2.76    0.65   0.5667   
CondBB2            -2.85      26.52   3.50   -0.11   0.9203   
CondAA2:CondBB2     1.26      32.56   8.05    0.04   0.9700   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) CndAA2 CndBB2
CondAA2     -0.809              
CondBB2     -0.790  0.680       
CndAA2:CBB2  0.550 -0.745 -0.750
```

其中，随机效应的结果如下，可以看到确实每个被试的首次注视时间是有差别的；但是这里看到相关系数为1或-1，说明数据量太少，不足以体现随机效应，这时候需要对模型进行简化（见 《随机斜率的取舍》部分）：
```
Random effects:
 Groups   Name            Variance Std.Dev. Corr             
 Item     (Intercept)      1622     40.3                     
 Sub      (Intercept)      1467     38.3                     
          CondAA2           794     28.2    -1.00            
          CondBB2           649     25.5    -1.00  1.00      
          CondAA2:CondBB2   363     19.1     1.00 -1.00 -1.00
 Residual                 10237    101.2                     
Number of obs: 183, groups:  Item, 64; Sub, 3     
```

固定效应的结果如下，这里是把`A1` 和 `B1`分别设为`CondA`和`CondB`的基线，然后`A2` 和 `B2` 分别和对应的基线比较。

```
Fixed effects:
                Estimate Std. Error     df t value Pr(>|t|)   
(Intercept)       268.08      27.69   2.10    9.68   0.0089 **
CondAA2            17.73      27.35   2.76    0.65   0.5667   
CondBB2            -2.85      26.52   3.50   -0.11   0.9203   
CondAA2:CondBB2     1.26      32.56   8.05    0.04   0.9700   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

注意，固定效应不是主效应和交互作用，要查看主效应和交互作用需要用`anova()`函数:
```
anova(fit1)
```

结果如下，看出主效应和交互作用都不显著。
```
Analysis of Variance Table of type III  with  Satterthwaite 
approximation for degrees of freedom
            Sum Sq Mean Sq NumDF DenDF F.value Pr(>F)
CondA         9869    9869     1  2.74   0.964   0.40
CondB          157     157     1  4.03   0.015   0.91
CondA:CondB     15      15     1  8.04   0.002   0.97
```

汇报结果的一般顺序是：1. 主效应和交互作用；2. 如果主效应或者交互作用显著，再汇报`contrasts`的结果，但是像这里每个因素只有两个水平，因此因素内`contrasts`的结果和主效应的结果是一样的；3.如果交互作用显著则需要进行简单效应分析。

### 随机斜率的取舍
在上面建立的模型中，包含随机斜率和随机截距，但是有两个问题：
* 有两个自变量，随机斜率的组合有很多种，如何选取适当地模型？
* 选取的模型可能无法收敛或者自由度溢出，这时如何简化模型？
  1. 无法收敛的情况：当输出下面的warning的时候，说明模型无法收敛，这时候需要简化模型，使其收敛：
  <img src = 'https://github.com/usplos/self-programming/blob/master/warning.png'>
  2. 自由度溢出的情况：当输出下面的错误时，说明自由度溢出（一般只Item上只有随机截距，因此这个问题几乎不会出现），这时候也需要简化模型，使其收敛：
  <img src = 'https://github.com/usplos/self-programming/blob/master/freedom.png'>

针对自由度溢出，需要**将错误提示里的随机斜率剔除**即可。

而针对模型无法收敛的问题，首先我们最终选取的模型要符合**两个标准：1.可以收敛；2.不能过度拟合。**

方法是：**首先，考虑全模型**，即如下命令：

```
fitA = lmer(data = data1, FFD ~ CondA * CondB + (1 + CondA*CondB | Sub) + (1|Item))
```

执行命令后，**如果无法收敛，第二步，增大模型的迭代次数**：
```
fitA = lmer(data = data1, FFD ~ CondA * CondB + (1 + CondA*CondB | Sub) + (1|Item), control = lmerControl(optCtrl = list(maxfun = 20000))) 
# 这里以20000次为标准，大多数两因素的模型迭代到20000次都是可以收敛；我曾试过3因素的全模型，在20000次迭代后，也是可以收敛的。
```

无论增大迭代次数后，全模型是否可以收敛，都查看其随机效应（如果设置到20000次仍然不能收敛，那么继续增大迭代次数也是不太可能收敛的）。

观察全模型的随机效应：
```
Random effects:
 Groups   Name            Variance Std.Dev. Corr             
 Item     (Intercept)      1622     40.3                     
 Sub      (Intercept)      1467     38.3                     
          CondAA2           794     28.2    -1.00            
          CondBB2           649     25.5    -1.00  1.00      
          CondAA2:CondBB2   363     19.1     1.00 -1.00 -1.00
 Residual                 10237    101.2                     
Number of obs: 183, groups:  Item, 64; Sub, 3
```

注意：1. 如果Corr的值如果为1，代表过度拟合了（有时大于0.9也被视为过度拟合），这时候需要将对应的随机斜率从模型中去掉；2. 过度拟合会导致模型的随机效应部分出现共线性，因此要查看Corr的结果，并将过度拟合的斜率去掉（**Corr在0.9以上视为过度拟合**）。

总结起来， **第三步：去除全模型中随机效应里过度拟合的斜率(Barr D. J., 2013)。** 这里需要注意，这里的例子里只有三名被试的数据，因此随机斜率的Corr会很大，基本上都是1，但是真实的数据分析中，
* 会出现介于0.9到1之间的Corr，0.9以上的可以认定为过度拟合；
* 可能会出现多个过度拟合的值，比如上面的6个Corr全是1，这种情况下：
  1. 先删除最高阶的交互作用，因为只要有最高阶的交互作用，删除其他作用对模型没有影响(Barr D. J., 2013)；
  2. 删除交互作用后，如果仍不能收敛，请继续删除，这时如果两个主效应的Corr都大于0.9，请先删除Corr较大的那个；
  3. 删除较大的主效应后如果仍然不能收敛，保留较大Corr的主效应，删除Corr较小的主效应（**如果两个主效应的Corr都大于0.9，不能直接删掉它们，因为随机斜率彼此相互影响，删掉一个，其他的Corr都会相应改变**），以此类推，直至探索到符合上面两条标准的模型。
  4. **进行随机斜率筛选的时候，请保持最大迭代次数和全模型相同**，这样方便比较模型的差异。

这里看到Sub的三个随机斜率都过度拟合了，因此移除它们：
```
fit2 = lmer(data = data1, FFD ~ CondA * CondB + (1 | Sub) + (1|Item))
```

移除后不代表完事儿了，第四步：检查简化后的模型和全模型是否有差别：
```
anova(fit2, fitA)

Data: data1
Models:
object: FFD ~ CondA * CondB + (1 | Sub) + (1 | Item)
..1: FFD ~ CondA * CondB + (1 + CondA * CondB | Sub) + (1 | Item)
       Df  AIC  BIC logLik deviance Chisq Chi Df Pr(>Chisq)
object  7 2247 2270  -1117     2233                        
..1    16 2264 2316  -1116     2232  0.98      9          1
```

看到，两个模型之间没有显著差别 (`p > 0.05` 即可，一般去除过度拟合的成分后的模型和全模型都没有显著差别)，简化后的模型为最终采用的模型。


### 调整固定因子比较基线
上面的固定效应的结果中，是以`CondA的`第一个水平，即A1作为基线，如果想人为设置比较基线，最稳定的方法是用`factor(..., levels = )`重新编码因子， 并设置水平的顺序，这里将第二个水平设为基线：
```
data1$CondA = factor(data1$CondA, levels = c('A2', 'A1'))
fit1 = lmer(data = data1, FFD ~ CondA * CondB + (1 | Sub) + (1|Item))
summary(fit1)
```

结果如下，看到固定因子的基线及其相应的数值都变化了，相应地可以修改`CondB`的基线
```
Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
  freedom [lmerMod]
Formula: FFD ~ CondA * CondB + (1 | Sub) + (1 | Item)
   Data: data1

REML criterion at convergence: 2203

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.875 -0.636 -0.223  0.435  3.364 

Random effects:
 Groups   Name        Variance Std.Dev.
 Item     (Intercept)  1696     41.2   
 Sub      (Intercept)   197     14.0   
 Residual             10325    101.6   
Number of obs: 183, groups:  Item, 64; Sub, 3

Fixed effects:
                Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)      286.668     18.011  13.370   15.92  4.5e-10 ***
CondAA1          -20.623     21.963 134.691   -0.94     0.35    
CondBB2           -2.000     21.417 138.129   -0.09     0.93    
CondAA1:CondBB2    0.148     30.691 136.830    0.00     1.00    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) CndAA1 CndBB2
CondAA1     -0.587              
CondBB2     -0.609  0.492       
CndAA1:CBB2  0.423 -0.718 -0.694
```

这种比较（和基线比较）只是`R`里的其中一种比较方式，`R`里的比较方式共有如下五种：

* `contrasts = contr.helmert`：第二个水平对照第一个水平，第三个水平对照前两个的均值，第四个水平对照前三个的均
值，以此类推

* `contrasts = contr.poly`：基于正交多项式的对照，用于趋势分析（线性、二次、三次等）和等距水平的有序因子

* `contrasts = contr.sum`：对照变量之和限制为0。也称作偏差找对，对各水平的均值与所有水平的均值进行比较

* `contrasts = contr.treatment`：各水平对照基线水平（默认第一个水平）。也称作虚拟编码。（这个是无序因子常用的编码形式，也是LMM常用的和默认的）

* `contrasts = contr.SAS`：类似于`contr.treatment`，只是基线水平变成了最后一个水平。

比如不想和基线比较，而是想看每种水平和总体的均值的偏差程度，需要设置成`contr.sum`的格式，可以用`options(contrasts = )`来调节（注意，`option()`设置的时候需要同时设置无序因子（各水平间只有顺序的差异，没有大小差异，无法比较大小）和有序因子（各水平间有确定的大小关系，可以比较大小）的比较方法，我们的实验中的因子几乎都是无序因子，因此只需要改变下面的`options( )`命令中的第一个比较方式即可）：

```
options(contrasts = c('contr.sum','contr.poly')) 
fit1 = lmer(data = data1, FFD ~ CondA * CondB + (1 | Sub) + (1|Item))
summary(fit1)
```

结果如下，看出两个条件的固定效应发生了相应改变（这里只展现基线以外的水平的结果，如果想查看基线水平的结果，请重新编码因子水平），其它的比较方法读者可自行尝试。
```
Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
  freedom [lmerMod]
Formula: FFD ~ CondA * CondB + (1 | Sub) + (1 | Item)
   Data: data1

REML criterion at convergence: 2209

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.875 -0.636 -0.223  0.435  3.364 

Random effects:
 Groups   Name        Variance Std.Dev.
 Item     (Intercept)  1696     41.2   
 Sub      (Intercept)   197     14.0   
 Residual             10325    101.6   
Number of obs: 183, groups:  Item, 64; Sub, 3

Fixed effects:
              Estimate Std. Error       df t value Pr(>|t|)    
(Intercept)   275.3936    12.2191   2.8784   22.54  0.00025 ***
CondA1         10.2747     7.6518 133.4824    1.34  0.18162    
CondB1          0.9628     7.7132 142.4418    0.12  0.90083    
CondA1:CondB1   0.0369     7.6729 136.6929    0.00  0.99617    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) CondA1 CondB1
CondA1      -0.019              
CondB1       0.022 -0.013       
CndA1:CndB1 -0.002  0.027 -0.031
```

### 简单效应分析
注意，**固定效应(Fixed Efeects)**， **主效应(Main Effects)** 和 **简单效应(Simple Effects)** 是三个不同的概念。固定效应是因素内某个水平和基线水平的差异，相当于t检验；主效应是操纵某个因素（自变量）对因变量的影响，相当于F检验，即使固定效应显著，主效应也不一定显著；简单效应是指自变量A在自变量B的不同水平上对因变量的影响，其本质也是主效应，但是是在另一个变量某个水平上的主效应。 虽然这里交互作用并不显著，但是还是要演示一下如何进行简单效应分析：
```
library(emmeans) # emmeans数据包可以对我们组涉及的模型进行简单效应分析，结果可读性较强
emmeans(fit1, pairwise~CondA|CondB) # 比较CondB的不同水平上CondA水平之间的差别
```

结果分为两部分，第一部分输出不同`CondB`水平下，`CondA`不同水平的均值、标准误、自由度、置信区间等信息，如下：
```
$emmeans
CondB = B1:
 CondA emmean SE df lower.CL upper.CL
 A2       287 18 13      248      326
 A1       266 19 15      226      306

CondB = B2:
 CondA emmean SE df lower.CL upper.CL
 A2       285 18 13      246      323
 A1       264 18 14      225      303

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95  
```

第二部分是显著性检验的结果，如下：
```
$contrasts
CondB = B1:
 contrast estimate SE  df t.ratio p.value
 A2 - A1        21 22 134     0.9  0.3514

CondB = B2:
 contrast estimate SE  df t.ratio p.value
 A2 - A1        20 21 135     1.0  0.3417
```

### Planed contrasts
以上做的是没有事先明确指向性地对比，但有时我们有明确的假设，有要明确比较的两个条件，这时要做 **planed contrasts**。为此需要对数据进行整理，将`CondA`和`CondB`整合为一个因子变量`Cond`，同时规定相应地因子水平：
```
data1 = data1 %>% mutate(Cond = paste(CondA,CondB, sep = '') %>% factor(levels = c('A1B1','A1B2','A2B1','A2B2')))
data1
# A tibble: 183 x 6
   Sub      Item   FFD CondA CondB Cond 
   <chr>   <int> <int> <fct> <fct> <fct>
 1 Sub_001    22   273 A2    B1    A2B1 
 2 Sub_001    25   205 A1    B1    A1B1 
 3 Sub_001    30   203 A2    B1    A2B1 
 4 Sub_001    53   646 A1    B1    A1B1 
 5 Sub_001    10   185 A2    B1    A2B1 
 6 Sub_001    63   373 A1    B2    A1B2 
 7 Sub_001     5   190 A1    B1    A1B1 
 8 Sub_001    13   219 A1    B1    A1B1 
 9 Sub_001    14   180 A2    B1    A2B1 
10 Sub_001    24   314 A2    B2    A2B2 

levels(data1$Cond) # 查看因子的顺序
	[1] "A1B1" "A1B2" "A2B1" "A2B2"
```

下一步，建立`contrasts`的矩阵。矩阵建立的规则是：如果有2个自变量，分别有`n`和`m`个水平，那么建立的矩阵有`n × m`行，比如这里的矩阵是2 × 2 = 4 行；每一列代表一个要比较的效应，在某列内对不同的条件赋予“权重”，以数字的形式赋予，要保证该列所有权重的总和为0。比如我们要比较`CondA`的主效应，因为`A1`为`Cond`的前两个水平，`A2`为后两个水平，因此建立如下的矩阵
```
CMA = matrix(c(1,1,-1,-1), nrow  = 4)
rownames(CMA) = levels(data1$Cond)
colnames(CMA) = 'MainCondA'
CMA

     MainCondA
A1B1         1
A1B2         1
A2B1        -1
A2B2        -1
```

之后建立模型，这时需要放的自变量和随机斜率是`Cond`，而不是`CondA*CondB`，同时要设置`contrast`选项：
```
fit1 = lmer(data = data1, FFD ~ Cond + (1|Sub) + (1|Item), contrasts = list(Cond = CMA))
summary(fit1)

Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
  freedom [lmerMod]
Formula: FFD ~ Cond + (1 | Sub) + (1 | Item)
   Data: data1

REML criterion at convergence: 2221

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.901 -0.634 -0.219  0.447  3.391 

Random effects:
 Groups   Name        Variance Std.Dev.
 Item     (Intercept)  1733     41.6   
 Sub      (Intercept)   197     14.0   
 Residual             10169    100.8   
Number of obs: 183, groups:  Item, 64; Sub, 3

Fixed effects:
              Estimate Std. Error     df t value Pr(>|t|)    
(Intercept)     275.36      12.21   2.91   22.56  0.00023 ***
CondMainCondA   -10.32       7.59 135.43   -1.36  0.17644    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr)
CondManCndA 0.018
```

这时有`CondA`的主效应了。相似地，可以对比`CondB`的主效应。也可以进行简单效应的planed contrasts，比如比较`CondA`的不同水平上，`CondB`的主效应：
```
CMS = matrix(c(1, -1, 0, 0,
               0, 0, 1, -1),
             nrow = 4)
rownames(CMS) = levels(data1$Cond)
colnames(CMS) = c('SimpleOnCondA1','SimpleOnCondA2')
CMS

     SimpleOnCondA1 SimpleOnCondA2
A1B1              1              0
A1B2             -1              0
A2B1              0              1
A2B2              0             -1
```

```
fit1 = lmer(data = data1, FFD ~ Cond + (1|Sub) + (1|Item), contrasts = list(Cond = CMS))
summary(fit1)

Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
  freedom [lmerMod]
Formula: FFD ~ Cond + (1 | Sub) + (1 | Item)
   Data: data1

REML criterion at convergence: 2215

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.780 -0.612 -0.230  0.484  3.453 

Random effects:
 Groups   Name        Variance Std.Dev.
 Item     (Intercept)  1563     39.5   
 Sub      (Intercept)   209     14.5   
 Residual             10475    102.3   
Number of obs: 183, groups:  Item, 64; Sub, 3

Fixed effects:
                   Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)         275.725     12.329   2.766   22.36  0.00033 ***
CondSimpleOnCondA1    1.338     11.106 143.240    0.12  0.90428    
CondSimpleOnCondA2    0.864     10.770 140.171    0.08  0.93617    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) CSOCA1
CndSmplOCA1 0.016        
CndSmplOCA2 0.014  0.005
```

比较`CondB`不同水平上`CondA`的主效应：
```
CMS2 = matrix(c(1, 0, -1, 0,
               0, 1, 0, -1),
             nrow = 4)
rownames(CMS2) = levels(data1$Cond)
colnames(CMS2) = c('SimpleOnCondB1','SimpleOnCondB2')
CMS2

     SimpleOnCondB1 SimpleOnCondB2
A1B1              1              0
A1B2              0              1
A2B1             -1              0
A2B2              0             -1
```

```
fit1 = lmer(data = data1, FFD ~ Cond + (1|Sub) + (1|Item), contrasts = list(Cond = CMS2))
summary(fit1)

Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of
  freedom [lmerMod]
Formula: FFD ~ Cond + (1 | Sub) + (1 | Item)
   Data: data1

REML criterion at convergence: 2213

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-1.893 -0.633 -0.218  0.445  3.382 

Random effects:
 Groups   Name        Variance Std.Dev.
 Item     (Intercept)  1713     41.4   
 Sub      (Intercept)   196     14.0   
 Residual             10249    101.2   
Number of obs: 183, groups:  Item, 64; Sub, 3

Fixed effects:
                   Estimate Std. Error    df t value Pr(>|t|)    
(Intercept)           275.4       12.2   2.9   22.58  0.00024 ***
CondSimpleOnCondB1    -10.4       10.9 135.3   -0.95  0.34479    
CondSimpleOnCondB2    -10.2       10.7 136.7   -0.96  0.33815    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) CSOCB1
CndSmplOCB1  0.014       
CndSmplOCB2  0.012 -0.002
```

### 广义混合线性模型
如果因变量不是连续变量，比如兴趣区内是否接受回视、是否从兴趣区内发出回视、是否跳读兴趣区等等，则需要用广义线性模型。

`R`中做GLMM(Genaralized Linear Mixed Model)用到的函数是:

`glmer(data = , formula = , family = ,...)`

其中 `family = ` 有多种不同的选择(**注意是字符型的**)，分别如下:
* `binomial` - `link = “logit”`（如果自变量为0，1变量，`family`要设置为`binomial`）;
* `gaussian` - `link = "identity"`;
* `gamma` - `link = "inverse"`;
* `inverse.gaussian` - `link = "1/mu^2"`;
* `poisson` - `link = "log"`;
* `quasi` - `link = "identity", variance = "constant"`;
* `quasibinomial` - `link = "logit"`;
* `quasipoisson` - `link = "log"`;

我们常用的是二项分布 (`binomial`) 和正态分布 (`gaussian`)，其余的很少见，所以不做介绍了
其他参数设置和`lmer()`是一样的。这里我们以兴趣区接受回视的情况为例，数据如下：

<img src = 'https://github.com/usplos/self-programming/blob/master/regin.png'>

建立模型如下：
```
fit3 = glmer(data = data2, ReginRight ~ Cond + (1|Sub) + (1|Item), family = 'binomial')
summary(fit3)

Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: ReginRight ~ Cond + (1 | Sub) + (1 | Item)
   Data: data2

     AIC      BIC   logLik deviance df.resid 
     254      273     -121      242      182 

Scaled residuals: 
   Min     1Q Median     3Q    Max 
-2.391 -0.884  0.418  0.769  1.383 

Random effects:
 Groups Name        Variance Std.Dev.
 Item   (Intercept) 6.63e-09 8.14e-05
 Sub    (Intercept) 2.52e-01 5.02e-01
Number of obs: 188, groups:  Item, 64; Sub, 3

Fixed effects:
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)   0.2895     0.3295    0.88    0.380  
Cond1        -0.4642     0.2629   -1.77    0.077 .
Cond2        -0.3080     0.2660   -1.16    0.247  
Cond3        -0.0994     0.2660   -0.37    0.709  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
      (Intr) Cond1  Cond2 
Cond1 -0.026              
Cond2 -0.017 -0.296       
Cond3 -0.013 -0.302 -0.309
convergence code: 0
```

`glmer()` 和 `lmer()` 只是因变量的类别不同，其他操作都是一样的（包括随机斜率取舍问题 \[改变迭代次数时，`lmerControl()` 改为 `glmerControl()`\]，简单效应分析，主效应和交互作用查看，调整因子水平，planed contrasts）。



## JAMOVI
JAMOVI是今年火起来的，基于`R`写的统计软件，具有干净整洁的鼠标操纵界面和漂亮的APA格式表格图片输出，
而且是免费、开源的，用户可根据自己的需要从library里下载相应的分析模块。

点击[这里](https://www.jamovi.org/download.html)下载，

点击[这里](https://www.jamovi.org/library.html)了解不同的分析模块。

具体的操作步骤请见[附录](https://github.com/usplos/self-programming/blob/master/%E9%99%84%E5%BD%95.pdf)。

## HLM/LMM自由度计算
参考[该文件](https://github.com/psychbruce/stats/blob/master/HLM%20df.pdf)。

## HLM/LMM 的其他相关/背景知识请见[这里](https://zhuanlan.zhihu.com/p/50048784)

## 参考文献
Barr, D. J.. (2013). Random effects structure for testing interactions in linear mixed-effects models. *Frontiers in Psychology, 4*.
