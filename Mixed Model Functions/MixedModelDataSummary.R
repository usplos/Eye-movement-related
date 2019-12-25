MixedModelDataSummary = function(Data, DV = 'DV',Cond = 'CondA', Group = 'subj'){
  if(!require(tidyverse)) install.packages('tidyverse')

  DF = eval(parse(text = paste0('Data %>% group_by(',
                                Cond,',',Group,')')))
  DF = eval(parse(text = paste0('summarise(DF, Mean2 = mean(',DV,'))')))
  DF = eval(parse(text = paste0('DF %>% group_by(',Cond,')')))
  DF = eval(parse(text = paste0('summarise(DF, Mean = mean(Mean2), SE = sd(Mean2)/sqrt(length(Mean2)))')))
  names(DF)[which(names(DF) == 'Mean')] = paste(DV, '_Mean',sep = '')
  names(DF)[which(names(DF) == 'SE')] = paste(DV, '_SE',sep = '')
  return(DF)
}
