SV = function(Data, bootstrapNumber = 10000, perbinMax = 600, perbinMin = 0, baseline,
              Ylab = 'DV', Xlab = 'IV', xp = perbinMax-(perbinMax-perbinMin)/5, Cex = 0.8, Bty = 'n',
             Width = 680, Height = 370)
{
  ########### package check
  PackageCheck = function(Name)
  {
    if (sum(unique(installed.packages()[, c("Package")] %in% 
                   Name)) == 0) {
      install.packages(Name)
    }
  }
  
  PackageCheck('dplyr');
  PackageCheck('purrr');
  PackageCheck('tidyr');
  PackageCheck('tibble');
  PackageCheck('ggplot2');
  PackageCheck('readr')
  ############ calculate
  library(dplyr);library(purrr);library(tidyr);library(tibble);library(readr);library(ggplot2);
  DataRaw = read_csv(Data)
  CondUnique = unique(DataRaw[[2]])
  
  for(i in CondUnique){
    eval(parse(text = paste(i,'perbin = matrix(0,nrow = bootstrapNumber, ncol = perbinMax-perbinMin+1)', sep = '')))
  }
  
  for(condition in CondUnique){
    CondTemp = matrix(0, nrow = bootstrapNumber, ncol = perbinMax - perbinMin+1)
    DataRawSS = filter(DataRaw ,DataRaw[[2]] %in% condition)
    SubjectUnique = unique(DataRawSS[[1]])
    for(subject in SubjectUnique)
    {
      DataRawS = DataRawSS %>% filter(DataRawSS[[1]] %in% subject)
      SubCondTemp = matrix(0, nrow = bootstrapNumber, ncol = perbinMax - perbinMin+1)
      for (i in 1:bootstrapNumber) 
        {
        SubCondTempOnce = sort(sample(DataRawS[[3]], size = nrow(DataRawS), replace = T))
        SubCondTempOnceProp = numeric()
          for(n in perbinMin:perbinMax)
          {
            checkn=which(perbinMin:perbinMax %in% n)
            SubCondTempOnceProp[checkn] = mean(SubCondTempOnce > n)
            
          }
        SubCondTemp[i,] = SubCondTempOnceProp
      }
      CondTemp = SubCondTemp+ CondTemp;
      cat(subject,' is done\n')
    }
    eval(parse(text = paste(condition,'perbin = CondTemp/length(SubjectUnique)', sep = '')))
    remove(CondTemp)
    cat(condition, ' is done\n###############################################################\n')
  }
  
    CondCompare = eval(parse(text = paste(CondUnique[-1 * which(CondUnique %in% baseline)],'perbin - ', baseline,'perbin', sep = '')));
    Differ = numeric(perbinMax - perbinMin +1)
    for(i in 1:(perbinMax - perbinMin +1))
    {
      Differ[i] = mean(CondCompare[,i]>0)
    }
    check = 0;TimePoint = NA;
    for (i in 1:(perbinMax - perbinMin +1-4)) {
      if(mean(Differ[i:(i+4)] > 0.95) == 1)
      {
        TimePoint = i;
        check = 1;break
      }
    }
    if(check == 1)
    {
      cat('The first time point is ',TimePoint,' ms.\n')
      eval(parse(text = paste(CondUnique[-1 * which(CondUnique %in% baseline)],'perbinA = numeric()', sep = '')))
      eval(parse(text = paste(baseline,'perbinA = numeric()', sep = '')))
      for(i in 1:(perbinMax - perbinMin + 1))
      {
        eval(parse(text = paste(CondUnique[-1 * which(CondUnique %in% baseline)],'perbinA[i] = mean(',CondUnique[-1 * which(CondUnique %in% baseline)],'perbin[,i])', sep = '')))
        eval(parse(text = paste(baseline,'perbinA[i] = mean(',baseline,'perbin[,i])', sep = '')))
      }
      
      eval(parse(text = paste('file = tibble(',baseline,' = ',baseline,'perbinA',', ',CondUnique[-1 * which(CondUnique %in% baseline)],' = ',CondUnique[-1 * which(CondUnique %in% baseline)], 'perbinA)',sep = '')))
      
      eval(parse(text = 'filename = paste(substr(Data, 1, nchar(Data)-4),\'_\',baseline, CondUnique[-1 * which(CondUnique %in% baseline)], \'.csv\',sep = \'\')'))
      write_csv(file, filename);write_csv(tibble(TIMEPOINT = TimePoint), paste(substr(Data,1,nchar(Data)-4),'_TimePoint.csv',sep = ''))
      bmp(paste(substr(Data, 1, nchar(Data)-4), '.bmp',sep = ''), width = Width, height = Height)
      eval(parse(text = paste('plot(',CondUnique[-1 * which(CondUnique %in% baseline)],'perbinA,type = \'l\', xlim = c(perbinMin, perbinMax), ylim = c(0,1), xlab = Xlab, ylab = Ylab)', sep = '')))
      eval(parse(text = paste('lines(',baseline,'perbinA, lty = 2)', sep = '')))
      eval(parse(text = paste('legend(xp, 0.95, cex = Cex, bty = Bty, legend =c(\'',CondUnique[-1 * which(CondUnique %in% baseline)],'\',\'',baseline,'\'), lty = c(1,2))',sep = '')))
      eval(parse(text = paste('abline(v = TimePoint, lty = 6)')))
      eval(parse(text = paste('text(x = TimePoint-45, y = 0.1, \'x = ',TimePoint,'\')', sep = '')))
      dev.off()
      cat('Survival analysis is finished\n')
    }
    if(check == 0)
    {
      cat('There is no seperation.\nSurvival analysis is finished\n',rep('#################',3))
    }
  
}
