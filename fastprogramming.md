# 快速编程(Eyetrack程序所用材料的格式转化)
Tips: 读者如果是做眼动的, 且用的是 Eye track 的, 且感兴趣的请继续阅读, 可能会有帮助; 如果不是, 就不要在这篇文章上耗费时间了

## 材料准备
材料整理成 [`M1.xlsx`](https://github.com/usplos/self-programming/raw/master/M1.xlsx) 文件的格式, 各列的含义分别为：

**T** - 实验材料的类别, 只能是严格的三个字符串之一,
即是实验材料(`experimental`)还是练习材料(`practice`)还是填充材料(`filler`),
如果是其他的字符, 或者有大写字符, 或者有空格, 会出错;

**C** - 实验条件, 比如是个单因素 3 水平的实验, C 里面只能有三个数字,
如果是个多因素, 比如 n \* m 的实验, 那么就会有 n \* m 个数字, 需要记住每个数字对应的条件都是什么;

**I** - 即 *Item*, 每个条件下不同 trial 的个数;

**D** - 是否需要按键反应, 需要为 `1`, 否则为 `0`;

**S&Q** - 句子或者问题（/选项）。

**B** - 即 *Button*, 反应的按键, 因为本人的实验多用手柄进行反应,
因此这里 demo 是左(`left`)右(`right`), 也可以设置为其它值(如果是其他值, 需要谨慎, 不知道能不能适用).

**需要注意:整理的文件的第一行的名字可以不和上面这个相同,
但是每一列代表的含义必须和上面严格对应(即第一列必须是材料的类别, 第二列必须是条件, 以此类推), 否则会出错.**

## 函数执行
将[`fastprogramming.R`](https://github.com/usplos/self-programming/blob/master/fastprogramming.R)
文件内的代码全部执行, 以创建`fastprogram()`函数. 该函数有三个参数:

`dirs` - 工作路径;

`inputname` - 已经整理好, 需要进行转化的文件的名字(一般为excel文件);

`outputname` - 转化后, 要输出的结果的文件名(最好是`txt`文件, 否则容易出现乱码).

`seq` - 默认为`T`, 表示材料和问题分为两个trial呈现，如果合在一个trial中呈现, 请设置为F.

以[`M1.xlsx`](https://github.com/usplos/self-programming/raw/master/M1.xlsx)为例, 执行以下命令:
```
fastprogram('/Users/balabala/Desktop',
            'M1.xlsx',
            'M1out.txt',
            #seq = T
            )
```

即可在工作路径下, 输出文件[`M1out.txt`](https://github.com/usplos/self-programming/blob/master/M1out.txt).

接下来将内容复制粘贴就可以了.
