
# 适用情形（持续更新中……）：
## 因变量为连续变量–单因素实验设计
## 因变量为连续变量–两因素实验设计
## 因变量为连续变量–三因素实验设计
## 因变量为二分变量–单因素实验设计(注意此时criterionPCA参数可能需要设置地更小一点，比如0.005 或 0.001)
## 因变量为二分变量-两因素实验设计

# 创建contr.simple()函数
source('https://raw.githubusercontent.com/usplos/Eye-movement-related/master/contr.simple.R')

# 创建生成虚拟变量的函数
ModelMatrix = function(Data, Fix_Factor, MatrixDesign = '*'){
  Data[Fix_Factor] = lapply(Data[Fix_Factor], factor)
  for(ff in Fix_Factor){
    contrasts(Data[[ff]]) = contr.simple(length(levels(Data[[ff]])))
  }

  eval(parse(text = paste0('mmff = model.matrix(~ ',paste0(Fix_Factor,collapse = MatrixDesign),', Data)')))
  IVName = gsub(pattern = ':',replacement = '_',x = colnames(mmff)[2:ncol(mmff)]) %>%
    substr(x = ., start = 1, stop = nchar(.))
  Data[IVName] = mmff[,2:ncol(mmff)]

  return(Data)
}

# 创建ModelOptimize_Factor()函数
ModelOptimize_Factor = function(FormulaManual = NULL,Data, DV, Fix_Factor, Re_Factor,
                                Family = 'gaussian', criterionPCA = 0.01, MatrixDesign = '*', REML = F){
  if(!require(tidyverse)) install.packages('tidyverse')
  if(!require(lmerTest)) install.packages('lmerTest')

  if(is.null(FormulaManual)){
    Data[Fix_Factor] = lapply(Data[Fix_Factor], factor)
    Data[Re_Factor] = lapply(Data[Re_Factor], factor)

    for(ff in Fix_Factor){
      contrasts(Data[[ff]]) = contr.simple(length(levels(Data[[ff]])))

      cat('For the fixed factor of ', ff, ', the contrasts matrix is:\n')
      print(contrasts(Data[[ff]]))
      cat('\n')
    }

    eval(parse(text = paste0('mmff = model.matrix(~ ',paste0(Fix_Factor,collapse = MatrixDesign),', Data)')))

    IVName = gsub(pattern = ':',replacement = '_',x = colnames(mmff)[2:ncol(mmff)]) %>%
      substr(x = ., start = 1, stop = nchar(.))
    Data[IVName] = mmff[,2:ncol(mmff)]
    RandomSlope = paste('(1 + ',paste(IVName,collapse = ' + '),'||',Re_Factor,')',sep = '')
    Formula = paste0(DV,' ~ 1 + ', paste(IVName,collapse = ' + '),' + ',
                     paste(RandomSlope,collapse = ' + '))
  }else{
    Formula = FormulaManual
  }


  if(Family == 'gaussian'){
    ModelAll = lmer(formula = as.formula(Formula), data = Data, REML = REML,
                    control = lmerControl(optimizer = "bobyqa"))


  }else{
    ModelAll = glmer(formula = as.formula(Formula), data = Data, REML = REML,
                     control = glmerControl(optimizer = "bobyqa"), family = Family)
  }

  if(!is.null(FormulaManual)){
    IVName = summary(ModelAll)$coef %>% row.names() %>% .[2:length(.)]
    DV = strsplit(x = FormulaManual, split = '~',fixed = T,) %>% unlist() %>% .[[1]]
  }

  PCA_All = summary(rePCA(ModelAll))
  k = 0;
  for (ii in 1:length(PCA_All)) {
    k = k + sum(PCA_All[[ii]]$importance[2,] < criterionPCA)
  }

  ModelOpt = ModelAll
  NumLoop = 0
  while (k != 0) {
    VarM = VarCorr(ModelOpt)
    NamesVarM = names(VarM)
    Group = names(VarM) %>% strsplit(x = ., split = '.', fixed = T) %>% unlist() %>% .[is.na(as.double(.))]
    Effect = c()
    Std = c()
    for(nn in 1:length(Group)){
      #nn=1
      Matrix = eval(parse(text = paste0('VarM$',NamesVarM[[nn]])))
      Effect[[nn]] = attributes(Matrix)$dimnames[[1]] %>% ifelse(test = .=='(Intercept)','1',.)
      Std[[nn]] = attributes(Matrix)$std
    }

    StdMatrix = data.frame(Group, Effect, Std)
    StdMatrixIntercept = subset(StdMatrix, Effect == '1')
    StdMatrixSlope = subset(StdMatrix, Effect != '1')
    StdMatrixSlope = StdMatrixSlope %>% arrange(-Std) %>% .[1:(nrow(.)-1),]
    StdMatrix = bind_rows(StdMatrixIntercept, StdMatrixSlope)

    RandomSlopeNew = StdMatrix %>% split(.$Group) %>% map_chr(function(df) {
      df = arrange(df, Effect);paste0('(',paste0(df$Effect, collapse = ' + '), ifelse(length(df$Effect) == 1,' | ',' || '), unique(df$Group),')')
    }) %>% unlist() %>% paste0(collapse = ' + ')

    FormulaNew = paste0(DV,' ~ 1 + ', paste(IVName,collapse = ' + '),' + ',
                        paste(RandomSlopeNew,collapse = ' + '))


    if(Family == 'gaussian'){
      ModelOpt = lmer(formula = as.formula(FormulaNew), data = Data, REML = REML,
                      control = lmerControl(optimizer = "bobyqa"))


    }else{
      ModelOpt = glmer(formula = as.formula(FormulaNew), data = Data, REML = REML,
                       control = glmerControl(optimizer = "bobyqa"), family = Family)
    }

    PCA_All = summary(rePCA(ModelOpt))
    k = 0;
    for (ii in 1:length(PCA_All)) {
      k = k + sum(PCA_All[[ii]]$importance[2,] < criterionPCA)
    }
    NumLoop = NumLoop+1
  }

  if(NumLoop > 0){
    if(is.null(FormulaManual)){
      cat('\n\n####################\n\nThe formula of the maximum model was below:\n\n',
          Formula,'\n\n')
      cat('The variance correlation matrix of the maximum model was:\n\n')
      print(VarCorr(ModelAll))
      cat('\n\n####################\n\n')

      cat('HOWEVER, we suggest you use the model with the following formula:\n\n',
          FormulaNew,'\n\n####################\n\n')

      cat('The differences between the maximum model and the optimized model was calculated with anova() function, and the results were shown:\n\n')
      print(anova(ModelAll, ModelOpt))
      cat('\n\n')

      return(list(DataNew = Data,
                  Formula_All = Formula,
                  Formula_Opt = FormulaNew,
                  VarCorr_All = VarCorr(ModelAll),
                  rePCA_All = summary(rePCA(ModelAll)),
                  VarCorr_Opt = VarCorr(ModelOpt),
                  rePCA_Opt = summary(rePCA(ModelOpt)),
                  Model_Compare = anova(ModelAll, ModelOpt),
                  Summary_ModelOpt = summary(ModelOpt)))
    }else{
      cat('\n\n####################\n\nThe formula of the model that you input was below:\n\n',
          Formula,'\n\n')
      cat('The variance correlation matrix of the given model was:\n\n')
      print(VarCorr(ModelAll))
      cat('\n\n####################\n\n')

      cat('HOWEVER, we suggest you use the model with the following formula:\n\n',
          FormulaNew,'\n\n####################\n\n')

      cat('The differences between the given model and the optimized model was calculated with anova() function, and the results were shown:\n\n')
      print(anova(ModelAll, ModelOpt))
      cat('\n\n')

      return(list(DataNew = Data,
                  Formula_Giv = Formula,
                  Formula_Opt = FormulaNew,
                  VarCorr_Giv = VarCorr(ModelAll),
                  rePCA_Giv = summary(rePCA(ModelAll)),
                  VarCorr_Opt = VarCorr(ModelOpt),
                  rePCA_Opt = summary(rePCA(ModelOpt)),
                  Model_Compare = anova(ModelAll, ModelOpt),
                  Summary_ModelOpt = summary(ModelOpt)))
    }


  }else{
    if(is.null(FormulaManual)){
      cat('\n\n####################\n\nThe maximum model was the most suggested:\n\n',
          Formula,'\n\n')
      cat('The variance correlation matrix of the maximum model was:\n\n')
      print(VarCorr(ModelAll))
      cat('\n\n')

      return(list(DataNew = Data,
                  Formula_All = Formula,
                  VarCorr_All = VarCorr(ModelAll),
                  rePCA_All = summary(rePCA(ModelAll)),
                  Summary_ModelAll = summary(ModelAll)))
    }else{
      cat('\n\n####################\n\nThe model that you input was the most suggested:\n\n',
          Formula,'\n\n')
      cat('The variance correlation matrix of the given model was:\n\n')
      print(VarCorr(ModelAll))
      cat('\n\n')

      return(list(DataNew = Data,
                  Formula_All = Formula,
                  VarCorr_All = VarCorr(ModelAll),
                  rePCA_All = summary(rePCA(ModelAll)),
                  Summary_ModelAll = summary(ModelAll)))
    }
  }
}

