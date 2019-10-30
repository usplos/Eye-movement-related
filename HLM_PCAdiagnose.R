HLM_PCAdiagnose = function(Model, Criterion=0.9){
  if(!require(crayon)){
    install.packages('crayon')
  }
  
  SummaryModel = summary(Model)
  PCAModel = rePCA(Model)
  
  NumRFactor = length(SummaryModel$varcor)
  NameRFactor = names(SummaryModel$varcor)
  NameRSlope = list()
  for(ii in 1:NumRFactor){
    NameRSlope[[ii]] = colnames(SummaryModel$varcor[[ii]])
  }
  
  for(ii in 1:NumRFactor){
    
    PCASummary = summary(eval(parse(text = paste0('PCAModel$',NameRFactor[[ii]]))))
    colnames(PCASummary$importance) = paste('Comp',1:ncol(PCASummary$importance))
    
    PCAMatrix = eval(parse(text = paste0('PCAModel$',NameRFactor[[ii]],'$rotation')))
    
    rownames(PCAMatrix) = NameRSlope[[ii]]
    colnames(PCAMatrix) = paste('Comp',1:ncol(PCAMatrix))
    cat(paste(rep('-------',10),collapse = ''),'\n')
    cat('For the cluster variable of', underline(bold(blue(NameRFactor[[ii]]))),', the ',underline(bold('raw PCA matrix ')),'was shown below:\n')
    print(round(PCAMatrix, digits = 3))
    cat(paste(rep('- -',10),collapse = ' '),'\n')
    
    
    
    cat('The', underline(bold('importance of the components')),'was shown below:\n')
    print(round(PCASummary$importance,3))
    
    cat(paste(rep('- -',10),collapse = ' '),'\n')
    cat('After the ', bold('FILTERING'),'according to the' ,bold(yellow('Criterion')),', the',  bold(underline('filtered components')),'was:\n')
    PCAMatrix[abs(PCAMatrix) < Criterion] =NA
    print(round(PCAMatrix, 3))
    cat(paste(rep('-------',10),collapse = ''),'\n')
  }
  
  cat('\n','We suggest the', underline(bold(green('intercept/slopes which were not filtered out'))), 'might be appropriate to be involved in the model','\n')
}
