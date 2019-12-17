source('https://raw.githubusercontent.com/usplos/Eye-movement-related/master/contr.simple.R')

ModelOptimize_Factor = function(Data, DV, Fix_Factor, Re_Factor,Family = 'gaussian', criterionPCA = 0.01){
  Data[Fix_Factor] = lapply(Data[Fix_Factor], factor)
  Data[Re_Factor] = lapply(Data[Re_Factor], factor)

  for(ff in Fix_Factor){
    contrasts(Data[[ff]]) = contr.simple(length(levels(Data[[ff]])))

    cat('For the fixed factor of ', ff, ', the contrasts matrix is:\n')
    print(contrasts(Data[[ff]]))
    cat('\n')
  }

  eval(parse(text = paste0('mmff = model.matrix(~ ',paste0(Fix_Factor,collapse = '/'),', Data)')))
  #mmff <- model.matrix(~ WholePI/EmbPI, ffRegVerb)
  #Ncol = ncol(Data)
  IVName = gsub(pattern = ':',replacement = '_',x = colnames(mmff)[2:ncol(mmff)]) %>%
    substr(x = ., start = 1, stop = nchar(.))
  Data[IVName] = mmff[,2:ncol(mmff)]
  RandomSlope = paste('(',paste(IVName,collapse = ' + '),'||',Re_Factor,')',sep = '')
  Formula = paste0(DV,' ~ ', paste(IVName,collapse = ' + '),' + ',
                   paste(RandomSlope,collapse = ' + '))
  if(Family == 'gaussian'){
    ModelAll = lmer(formula = as.formula(Formula), data = Data, REML = F,
                    control = lmerControl(optimizer = "bobyqa"))


  }else{
    ModelAll = glmer(formula = as.formula(Formula), data = Data, REML = F,
                     control = lmerControl(optimizer = "bobyqa"), family = Family)
  }

  PCA_All = summary(rePCA(ModelAll))
  k = 0;
  for (ii in 1:length(Re_Factor)) {
    k = k + sum(PCA_All[[ii]]$importance[2,] < criterionPCA)
  }

  ModelOpt = ModelAll
  while (k != 0) {
    VarM = VarCorr(ModelOpt)
    NamesVarM = names(VarM)
    Group = names(VarM) %>% strsplit(x = ., split = '.', fixed = T) %>% unlist() %>% .[is.na(as.double(.))]
    Effect = c()
    Std = c()
    for(nn in 1:length(Group)){
      #nn=1
      Matrix = eval(parse(text = paste0('VarM$',NamesVarM[[nn]])))
      Effect[[nn]] = attributes(Matrix)$dimnames[[1]]
      Std[[nn]] = attributes(Matrix)$std
    }

    StdMatrix = data.frame(Group, Effect, Std)
    StdMatrix = subset(StdMatrix, Effect != '(Intercept)') %>% arrange(-Std) %>% .[1:(nrow(.)-1),]

    RandomSlopeNew = StdMatrix %>% split(.$Group) %>% map_chr(function(df) {
      paste0('(',paste0(df$Effect, collapse = ' + '),' || ', unique(df$Group),')')
    }) %>% unlist() %>% paste0(collapse = ' + ')

    FormulaNew = paste0(DV,' ~ ', paste(IVName,collapse = ' + '),' + ',
                        paste(RandomSlopeNew,collapse = ' + '))


    if(Family == 'gaussian'){
      ModelOpt = lmer(formula = as.formula(FormulaNew), data = Data, REML = F,
                      control = lmerControl(optimizer = "bobyqa"))


    }else{
      ModelOpt = glmer(formula = as.formula(FormulaNew), data = Data, REML = F,
                       control = lmerControl(optimizer = "bobyqa"), family = Family)
    }

    PCA_All = summary(rePCA(ModelOpt))
    k = 0;
    for (ii in 1:length(Re_Factor)) {
      k = k + sum(PCA_All[[ii]]$importance[2,] < criterionPCA)
    }
  }

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

  return(Data)
}
