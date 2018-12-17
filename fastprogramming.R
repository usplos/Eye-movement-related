fastprogram = function(dirs,inputname, outputname, seq = T)
{
  #dirs - a string of the working direction
  #inputname - a string of the file name of the file you want to transform
  #outputname - a string of the file name of the file you want to output
  #the current function need rio packasges, so please be sure that the package has been downloaded with command <install.packages('rio')>
  #if you have not installed the rio package, never mind because it will check whether you have installed the package and would install it if there were not this package
  if(sum(unique(installed.packages()[,c('Package')] %in% 'rio')) == 0)
  {install.packages('rio')}
  setwd(dirs)
  library(rio)
  rawdata = import(inputname)
  output = c()
  for(i in 1:nrow(rawdata))
  {
    temp = rawdata[i,]
    if(temp[1,4] == 0)
    {
      temp1 = temp[1,1]
      V1 = paste("trial ", 
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],
                 "D",temp[1,4],
                 sep = "")
      V2 = "  gc_rect =          (0 0 0 0)"
      V3 = paste("  inline =           |",temp[1,5],sep = "")
      V4 = "  max_display_time = 20000"
      V5 = paste("  trial_type =        ",temp[1,1],sep = '')
      V6 = paste("end ", 
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],
                 "D",temp[1,4],
                 sep = "")
      
      output = rbind(output,V1,V2,V3,V4,V5,V6,"")
    }
    else
    {
      temp1 = temp[1,1]
      V1 = paste("trial ", 
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],
                 "D",temp[1,4],
                 sep = "")
      V2 = "  gc_rect =          (0 0 0 0)"
      V3 = paste("  inline =           |",temp[1,5],sep = "")
      V4 = "  max_display_time = 20000"
      V5 = paste("  trial_type =        ",'question',sep = '')
      V6 = paste("end ", 
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],
                 "D",temp[1,4],
                 sep = "") 
      V7 = paste("  button =           ",temp[1,6],"Trigger",sep = "")
      output = rbind(output,V1,V7,V2,V3,V4,V5,V6,"")
    }
  }
  SEQ = which(rawdata$D == 1)
  if(seq == T)
  {
    for(i in SEQ)
    {
      temp = rawdata[i,]
      temp1 = temp[1,1]
      V1 = paste('sequence S',
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],
                 sep = "")
      V2 = paste(' ',
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],"D0",
                 sep = '')
      V3 = paste(' ',
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],"D1",
                 sep = '')
      V4 = paste('end S',
                 ifelse(temp1 == "experimental", "E", ifelse(temp1 == "practice"  , "P", "F")),
                 temp[1,2],"I", 
                 temp[1,3],
                 sep = "")
      output = rbind(output, V1, V2, V3, V4, "")
    }
  }
  write.csv(output,outputname,row.names = F,quote = F)
}

