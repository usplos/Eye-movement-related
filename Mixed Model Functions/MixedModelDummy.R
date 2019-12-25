ModelMatrix = function(Data, Fix_Factor, MatrixDesign = '*'){
  Fix_Factor = Fix_Factor %>% strsplit(split = ',', fixed = T) %>% unlist()
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
