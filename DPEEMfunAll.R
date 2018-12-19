###### package check
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

library(dplyr);library(purrr);library(tidyr);library(tibble);library(readr);library(ggplot2);

##### SV
funSV = function(Data, bootstrapNumber = 10000, perbinMax = 600, perbinMin = 0, baseline,
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

###### DA12csv
funDA12csv <-
  function()
  {
    # convert the DA1 files to csv files
    # the DA1 files should be names as Sub*.DA1
    # the DA1 files should be in current working directory
    
    DA1filename = dir(pattern = '[Ss]*')
    DA1filename = DA1filename[grep('*.DA1',DA1filename)]
    if(length(DA1filename) == 0)
    {cat('Error: there is no DA1 file\n')}else
    {
      for (ida1 in DA1filename) {
        icsv = paste(substr(ida1,1,nchar(ida1)-4),'.csv',sep = '')
        file.copy(ida1, icsv)
        cat(ida1,'is done\n')
      }
      cat('DA1 to csv convert is done!\n\n')
      
    }
  }

###### preprocessing
funpreprocess <-
  function(workdir, FDMax = 1000, FDMin = 80, ROIfilename, outputdir)
  {
    # This function is aimed to tide the csv files of each subjects.
    # Files of all subjects will be tidied in to FTtotal.csv in which one line contains the basic information about one fixation point.
    # FTtotalA.csv will then be produced based on FTtotal.csv in which several variables is appended inclusing the final coordinate (xcoor + ycoor*160), whether it is the first-pass fixation, fixation duration, whether it is in the ROI, saccade direction from current fixation point.
    # FTtotalAS.csv will then be produced based on FTtotalA.csv in which fixations whose duration located within the limitation will be remained.
    # FTtotalASR.csv will then be produced based on FTtotalAS.csv in which only fixations locating in ROI are remained.
    # FTtotalASRpt.csv will then be produced based on FTtoalAS.csv in which another variable is appended which indicats how many times the current fixation point has passed the ROI if it is located in ROI.
    # FTtotalASRptReg.csv will finally be produced based on FTtotalASRptReg.csv in which another variable is appended which indicates whether the current saccade to ROIs is from right or left to ROIs.
    # workdir - a character string indicating where the csv files are
    # FDMax - numeric indicating the longest duration of fixation point you want to remain
    # FDMin - numeric indicating the shortest duration of fixation point you want to remain
    # ROIfilename - a character string indicating the name ROI location file
    # outputdir - a character string indicating where you want to put out the FTtotal*.csv files
    
    setwd(workdir)
    cat('Now preprocessing is done...\n')
    
    csvfilename = dir(pattern = "Sub*")
    csvfilename = csvfilename[grep('*.csv',csvfilename)]
    sub0 = c()
    item0 = c()
    cond0 = c()
    xcoor0 = c()
    ycoor0 = c()
    Tstart0 = c()
    Tend0 = c()
    
    for(i in csvfilename)
    {
      tempsubfile = read.csv(i, sep = ' ', header = F)
      
      for(j in 1:nrow(tempsubfile))
      {
        templine = tempsubfile[j,]
        templine = templine[,!is.na(templine)]
        if(ncol(templine) > 8 | templine[[3]] > 99)
        {
          if(templine[[3]] < 99)
          {
            numberfixation = (ncol(templine)-8)/4
            
            for(k in 1:numberfixation)
            {
              sub0 = c(sub0, substr(i,1,nchar(i)-4))
              cond0 = c(cond0, templine[[2]])
              item0 = c(item0, templine[[3]])
              xcoor0 = c(xcoor0, templine[[4*k+5]])
              ycoor0 = c(ycoor0, templine[[4*k+6]])
              Tstart0 = c(Tstart0, templine[[4*k+7]])
              Tend0 = c(Tend0, templine[[4*k+8]])
            }
          }
          else
          {
            numberfixation = ncol(templine)/4
            for(k in 1:numberfixation)
            {
              sub0 = c(sub0, substr(i,1,nchar(i)-4))
              cond0 = c(cond0, templine[j-1,][[2]])
              item0 = c(item0, tempsubfile[j-1,][[3]])
              xcoor0 = c(xcoor0, templine[[4*k-3]])
              ycoor0 = c(ycoor0, templine[[4*k-2]])
              Tstart0 = c(Tstart0, templine[[4*k-1]])
              Tend0 = c(Tend0, templine[[4*k]])
            }
          }
        } 
      }
      cat("\015")
      cat(i,' has been done!!',"\n")
    }
    
    FTtotal = data.frame(sub0,cond0,item0,xcoor0,ycoor0,Tstart0,Tend0, stringsAsFactors = F)
    naposition = which(is.na(FTtotal$cond0))
    for(i in naposition)
    {
      FTtotal$cond0[i] = FTtotal$cond0[i-1]
    }
    largeitemp = which(FTtotal$item0 > 99)
    for(i in largeitemp)
    {
      FTtotal$item0[i] = FTtotal$item0[i-1]
    }
    
    write.csv(FTtotal,paste(outputdir,'FTtotal.csv',sep = '/'),row.names = F, quote = F)
    FTtotal = read.csv(paste(outputdir,'FTtotal.csv',sep = '/'), stringsAsFactors = F)
    cat('FTtotal.csv has been produced','\n')
    
    
    FTtotal = within(FTtotal,{
      finalcoor = NA
      finalcoor = xcoor0+ycoor0*160
      finalcoor = as.integer(finalcoor)
    })
    
    subindex = unique(FTtotal$sub0)
    ffd0 = c()
    FFT = c()
    ROI0 = c()
    sacdir = c()
    checkffd = c()
    library(rio)
    ROI = import(ROIfilename)
    ROI[is.na(ROI),is.na(ROI)]=-1
    for(i in subindex)
    {
      ffd1 = length(ffd0)
      cat(i,' is doing...............','\n')
      tempsub = FTtotal[FTtotal$sub0 %in% i,]
      #shouldlength = nrow(tempsub)
      #checklength1 = length(ffd0)
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        
        ffd1 = length(ffd0)
        #cat(j,' is doing\n')
        # n == 1
        tempitem = tempsub[tempsub$item0 %in% j,]
        #shouldlength = nrow(tempitem)
        #checklength1 = length(ffd0)
        xmax = 0
        if(nrow(tempitem) > 1)
        {
          templine1 = tempitem[1,]
          templine2 = tempitem[2,]
          xmax = templine1$finalcoor[[1]]
          #ffd0
          ffd0 = c(ffd0, T)
          #FFT
          FFT = c(FFT, templine1$Tend0[[1]] - templine1$Tstart0[[1]])
          #sacdir
          x1 = templine1$finalcoor[[1]]
          x2 = templine2$finalcoor[[1]]
          sacdir = c(sacdir, ifelse(x2 > x1, 'fore','back'))
          #ROI0
          temproi = ROI[ROI$item == templine1$item0 & ROI$condition == templine1$cond0,][3:ncol(ROI)]
          ROI0 = c(ROI0, ifelse(x1 %in% temproi, T, F))
          #2 - n
          fnumber = nrow(tempitem)
          if(fnumber>1)
          {
            for(k in 2:fnumber)
            {
              templinen = tempitem[k,]
              xn = templinen$finalcoor[[1]]
              #ffd0
              if(xn > xmax)
              {
                ffd0 = c(ffd0,T)
                xmax = xn
              }
              else
              {
                ffd0 = c(ffd0,F)
              }
              #FFT
              FFT = c(FFT, templinen$Tend0[[1]] - templinen$Tstart0[[1]])
              #sacdir
              if(k < fnumber)
              {
                templinenp1 = tempitem[k+1,]
                xp1 = templinenp1$finalcoor[[1]]
                sacdir = c(sacdir, ifelse(xp1 > xn, 'fore', 'back'))
              }
              else
              {
                sacdir = c(sacdir, "NO")
              }
              #ROI
              temproi = ROI[ROI$item == templinen$item0 & ROI$condition == templinen$cond0,][3:ncol(ROI)]
              ROI0 = c(ROI0, ifelse(xn %in% temproi, T, F))
            }
          }
        }
        else
        {
          ffd0 = c(ffd0,'T')
          templine1 = tempitem[1,]
          xn = templine1$finalcoor[[1]]
          FFT = c(FFT, templine1$Tend0[[1]] - templine1$Tstart0[[1]])
          sacdir = c(sacdir,'NO')
          temproi = ROI[ROI$item == templinen$item0 & ROI$condition == templinen$cond0,][3:ncol(ROI)]
          ROI0 = c(ROI0, ifelse(xn %in% temproi, T, F))
        }
        checkffd = c(checkffd, length(ffd0) - ffd1)
        
      }
      #checklength2 = length(ffd0)
      #if((checklength2 - checklength1) != shouldlength)
      #{
      #  break()
      #}
      cat(i,' has been done!!!!','\n')
    }
    
    FTtotalA = cbind(FTtotal, ffd0, FFT, ROI0, sacdir)
    write.csv(FTtotalA, paste(outputdir, 'FTtotalA.csv',sep = '/'), row.names = F, quote = F)
    cat('FTtotalA.csv has been produced','\n')
    
    
    FTtotalAS = FTtotalA[FTtotalA$FFT %in% FDMax:FDMin,]
    write.csv(FTtotalAS, paste(outputdir, 'FTtotalAS.csv', sep = '/'),row.names = F, quote = F)
    cat('FTtotalAS.csv has been produced','\n')
    
    
    FTtotalASR = FTtotalAS[FTtotalAS$ROI0 == T,]
    write.csv(FTtotalASR, paste(outputdir,'FTtotalASR.csv',sep = '/'), row.names = F, quote = F)
    cat('FTtotalASR.csv has been produced','\n')
    cat('Preprocessing has been done','\n\n')
    
    
    cat('Calculating the ROI passing times...\n')
    
    FTtotalAS = read.csv(paste(outputdir, 'FTtotalAS.csv', sep = '/'), stringsAsFactors = F)
    FTtotalASRpt = within(FTtotalAS,{
      passtimes = NA
      passtimes[ROI0 == F] = 0
    })
    subindex = unique(FTtotalAS$sub)
    passtimes = c()
    for(i in subindex)
    {
      tempsub = FTtotalASRpt[FTtotalASRpt$sub0 %in% i,]
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        tempitem = tempsub[tempsub$item0 %in% j,]
        NAposition = which(is.na(tempitem$passtimes))
        if(length(NAposition) >0 )
        {
          passtimes = c(passtimes,1)
          count = 1
          if(length(NAposition) >1)
          {
            for(k in 2:length(NAposition))
            {
              if(NAposition[k]-NAposition[k-1] == 1)
              {
                passtimes = c(passtimes,count)
              }
              else
              {
                count = count+1
                passtimes = c(passtimes, count)
              }
            }
          }
        }
      }
      cat(i,' has been done!!!','\n')
    }
    FTtotalASRpt$passtimes[which(is.na(FTtotalASRpt$passtimes))] = passtimes
    write.csv(FTtotalASRpt, paste(outputdir, 'FTtotalASRpt.csv', sep = '/'), quote = F, row.names = F)
    cat('FTtotalASRpt.csv has been done','\n\n')
    
    cat('Calculating whether regression was existed...\n')
    
    FTtotalASRpt = read.csv(paste(outputdir,'FTtotalASRpt.csv', sep = '/'), stringsAsFactors = F)
    FTtotalASRptReg = within(FTtotalASRpt, {
      regressionfrom = NA
      regressionfrom[passtimes %in% 0] = 'None'
    })
    regression = c()
    subindex = unique(FTtotalASRpt$sub)
    for(i in subindex)
    {
      tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        tempitem = tempsub[tempsub$item0 %in% j,]
        NAposition = which(is.na(tempitem$regre))
        if(length(NAposition) > 0)
        {
          for(k in NAposition)
          {
            if(k==1)
            {
              regression = c(regression,'None')
            }
            else
            {
              if(tempitem$sacdir[[k-1]] %in% 'fore' & tempitem$ROI0[[k-1]] == F)
              {
                regression = c(regression,'Left')
              }
              else if(tempitem$sacdir[[k-1]] %in% 'back' & tempitem$ROI0[[k-1]] == F)
              {
                regression = c(regression,'Right')
              }
              else
              {
                regression = c(regression,regression[length(regression)])
              }
            }
          }
        }
      }
      cat(i,' has been done!!!','\n')
    }
    FTtotalASRptReg$regressionfrom[is.na(FTtotalASRptReg$regressionfrom)] = regression
    write.csv(FTtotalASRptReg, paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), row.names = F, quote = F)
    cat('FTtotalASRptReg.csv has been done','\n\n')
    
  }

###### FFD & TT Refined
funROITTFFD <-
  function(outputdir)
  {
    # calculate the first fixation duration and total time in ROI on each trial in each subject
    # based on FTtoalASR.csv
    # outputdir - a character string indicating where you want to put out the result file
    # FTtotalASR.csv should be in current working directory
    
    cat('Calculating the first fixation duration and total time...\n')
    
    read_csv('FTtotalASRptReg.csv') %>% 
    mutate(Sub = sub0, Cond = cond0, Item = item0) %>%
    filter(ROI0 == T) %>% 
    group_by(Sub, Cond, Item) %>% 
    mutate(ROIFI = 1:length(FFT)) %>% 
    select(-xcoor0, -ycoor0, -Tstart0, -Tend0) %>% 
    summarise(TotalTime = sum(FFT), FFD = FFT[ROIFI == 1]) %>%
    write_csv('ROITT&FFD.csv')
    
    cat('Totaltime and first fixation duration have been done','\n\n')
    
  }

###### ROInum refined
funFTROInum <-
  function(outputdir)
  {
    # calculate the fixation numbers and its proportion in ROI for each trial on each prticipant
    # based on FTtotalAS.csv
    # outputdir - a character string indicating where you want to put out the result file
    # FTtotalAS.csv should be in current working directory
    
    cat('Calculating the number of fixation potint in ROI...\n')
    
    read_csv('FTtotalASRptReg.csv') %>% 
    mutate(Sub = sub0, Cond = cond0, Item = item0) %>%
    group_by(Sub, Cond, Item) %>% 
    summarise(FixationNum = length(ROI0[ROI0 == T]), 
            FixationProp = length(ROI0[ROI0 == T])/length(ROI0)) %>%
    write_csv('ROIFTnum.csv')
    
    cat('FTROInum.csv has been produced','\n\n')
    
  }

###### fixationprop refined
funROIfixationprop <-
  function(outputdir)
  {
    # calculate whether the ROI was focused when subject passed it first time
    # based on FTtotalASRptReg.csv file
    # outputdir - a character string where you want to put out the result file
    # FTtotalASRptReg.csv should be in current working directory
    
    cat('Calculating the fixation proportion...\n')
    
    read_csv('FTtotalASRptReg.csv') %>% 
    mutate(Sub = sub0, Cond = cond0, Item = item0) %>%
    filter(ROI0 == T) %>%
    group_by(Sub, Cond, Item) %>% 
    mutate(FixationIndex = 1:length(ROI0)) %>%
    summarise(FixationProp = length(ffd0[FixationIndex == 1 & ffd0 == T])) %>%
    write_csv('ROIFixationProp.csv')

    cat('ROI fixation proportion has been done','\n\n')
    
  }

###### gazeduration
funROIgazeduration <-
  function(outputdir)
  {
    # calcualte the gaze duration on ROI
    # based on FTtotalASRptReg.csv file
    # outputdir - a character string indicating where you want to put out the result files
    # FTtotalASRptReg.csv should be in current working directory
    
    cat('Calculating the first pass time...\n')
    
    read_csv('FTtotalASRptReg.csv') %>% 
  mutate(Sub = sub0, Cond = cond0, Item = item0) %>%
  filter(passtimes == 1) %>%
  group_by(Sub, Cond, Item) %>% 
  summarise(GazeDuration = sum(FFT)) %>%
  write_csv('ROIGazeDuration.csv')

    cat('ROI gaze duration has been done','\n\n')
    
  }

###### regression in
funROIregressionin <-
  function(outputdir)
  {
    # calculate whether the ROI received regression and whether the first time landing in ROI was regression
    # based on FTtotalASRepReg.csv
    # outputdir - a character string indicating where you want put out the result file
    # FTtotalASRptReg.csv should be in current working directory
    
    cat('Calculating the ROI regression in...\n')
    
    FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
    Sub = c()
    Item = c()
    Cond = c()
    ReginRight = c()
    ReginRightFF = c()
    subindex = unique(FTtotalASRptReg$sub)
    for(i in subindex)
    {
      tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        tempitem = tempsub[tempsub$item0 %in% j,]
        Sub=c(Sub,i)
        Item = c(Item,j)
        Cond = c(Cond, unique(tempitem$cond0)[[1]])
        if(sum(tempitem$regressionfrom[tempitem$passtimes != 0] %in% 'Right') != 0)
        {
          ReginRight = c(ReginRight, 1)
        }
        else
        {
          ReginRight = c(ReginRight, 0)
        }
        if(sum(tempitem$regressionfrom[tempitem$passtimes == 1] %in% 'Right') !=0)
        {
          ReginRightFF = c(ReginRightFF, 1)
        }
        else
        {
          ReginRightFF = c(ReginRightFF,0)
        }
      }
      cat(i,' has been done!!!','\n')
    }
    ROIrightregressionin = data.frame(Sub, Item, Cond, ReginRight, ReginRightFF, stringsAsFactors = F)
    write.csv(ROIrightregressionin, paste(outputdir, 'ROIrightregressionIn.csv', sep = '/'), row.names = F, quote = F)
    cat('ROI regression has been done','\n\n')
    
  }

###### regression out
funROIregressionout <-
  function(outputdir)
  {
    # calculate whether any regression was given out from ROI and whether any regression was given out from ROI first time passing
    # based on FTtotalASRptReg.csv file
    # outputdir - a character string indicating where you want to put out the result file
    # FTtotalASRptReg.csv should be in current working directory
    
    cat('Calculating the ROI regression out...\n')
    
    FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
    Sub = c()
    Item = c()
    Cond = c()
    Regressionout = c()
    FFregressionout = c()
    subindex = unique(FTtotalASRptReg$sub)
    for(i in subindex)
    {
      tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        tempitem = tempsub[tempsub$item0 %in% j,]
        Sub=c(Sub,i)
        Item = c(Item,j)
        Cond = c(Cond, unique(tempitem$cond0)[[1]])
        NAposition = which(tempitem$passtimes != 0)
        regout = 0
        if(length(NAposition)>0)
        {
          
          for(k in NAposition)
          {
            if(k < nrow(tempitem) & tempitem$passtimes[k+1] == 0 & tempitem$sacdir[k] %in% 'back')
            {
              regout = 1
            }
          }
        }
        Regressionout = c(Regressionout, regout)
        NAposition = which(tempitem$passtimes == 1)
        FTregout = 0
        if(length(NAposition) > 0)
        {
          for(k in NAposition)
          {
            if(k < nrow(tempitem) & tempitem$passtimes[k+1] == 0 & tempitem$sacdir[k] %in% 'back')
            {
              FTregout = 1
            }
          }
        }
        FFregressionout = c(FFregressionout, FTregout)
      }
      cat(i,'has been done!!!','\n')
    }
    ROIregressionout = data.frame(Sub, Item,Cond,Regressionout, FPregressionout = FFregressionout, stringsAsFactors = F)
    write.csv(ROIregressionout, paste(outputdir,'ROIregressionout.csv', sep = '/'), row.names = F, quote = F)
    cat('ROI regression out has been done','\n\n')
    
  }

###### saccade length
funROIsaccadelength <-
  function(outputdir)
  {
    # calculate the saccade length saccading in and out ROI when first time passing based on the direction of saccade from
    # based on FTtotalASRptReg.csv file
    # outputdir - a character string indicating where you want to put out the result file
    # FTtotalASRptReg.csv should be in current working directory
    
    cat('Calculating the saccade length...\n')
    
    FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
    Sub = c()
    Item = c()
    Cond = c()
    saccadelengthinL = c()
    saccadelengthinR = c()
    saccadelengthoutL = c()
    saccadelengthoutR = c()
    subindex = unique(FTtotalASRptReg$sub)
    for(i in subindex)
    {
      tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        tempitem = tempsub[tempsub$item0 %in% j,]
        Sub=c(Sub,i)
        Item = c(Item,j)
        Cond = c(Cond, unique(tempitem$cond0)[[1]])
        FFTposition0 = which(tempitem$passtimes == 1)
        if(length(FFTposition0)> 0)
        {
          FFTposition = FFTposition0[1]
          
          if(FFTposition > 1)
          {
            if(tempitem$sacdir[FFTposition-1] %in% 'fore')
            {saccadelengthinL = c(saccadelengthinL,sqrt((tempitem$xcoor0[FFTposition-1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition-1]-tempitem$ycoor0[FFTposition])^2))}
            else
            {saccadelengthinL = c(saccadelengthinL,NA)}
          }else
          {saccadelengthinL = c(saccadelengthinL,NA)}
          
          if(FFTposition > 1)
          {
            if(tempitem$sacdir[FFTposition-1] %in% 'back')
            {saccadelengthinR = c(saccadelengthinR,sqrt((tempitem$xcoor0[FFTposition-1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition-1]-tempitem$ycoor0[FFTposition])^2))}
            else
            {saccadelengthinR = c(saccadelengthinR,NA)}
          }else
          {saccadelengthinR = c(saccadelengthinR,NA)}
          
          FFTposition = FFTposition0[length(FFTposition0)]
          
          if(FFTposition < nrow(tempitem))
          {
            if(tempitem$sacdir[FFTposition] %in% 'fore')
            {saccadelengthoutR = c(saccadelengthoutR,sqrt((tempitem$xcoor0[FFTposition+1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition+1]-tempitem$ycoor0[FFTposition])^2))}
            else
            {saccadelengthoutR = c(saccadelengthoutR,NA)}
          }else
          {saccadelengthoutR = c(saccadelengthoutR,NA)}
          
          if(FFTposition < nrow(tempitem))
          {
            if(tempitem$sacdir[FFTposition] %in% 'back')
            {saccadelengthoutL = c(saccadelengthoutL,sqrt((tempitem$xcoor0[FFTposition+1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition+1]-tempitem$ycoor0[FFTposition])^2))}
            else
            {saccadelengthoutL = c(saccadelengthoutL,NA)}
          }else
          {saccadelengthoutL = c(saccadelengthoutL,NA)}
          
        }else
        {
          saccadelengthinL = c(saccadelengthinL,NA)
          saccadelengthinR = c(saccadelengthinR,NA)
          saccadelengthoutL = c(saccadelengthoutL, NA)
          saccadelengthoutR = c(saccadelengthoutR, NA)
        }
        
      }
      cat(i,'has been done!!!','\n')
    }
    ROIsaccadelength = data.frame(Sub, Item, Cond,saccadelengthinL,saccadelengthinR,saccadelengthoutL,saccadelengthoutR, stringsAsFactors = F)
    write.csv(ROIsaccadelength, paste(outputdir,'ROIsaccadelength.csv', sep = '/'), row.names = F, quote = F,na = '')
    cat('ROI saccade length is done\n\n')
    
  }

###### second fixation time
funROIsecondFT <-
  function(outputdir)
  {
    # calculate the duration of the second, third and four times passing the ROI
    # based on FTtotalASRepReg.csv
    # outputdir - a character string indicating where you want to put out the result file
    # FTtotalASRptReg.csv should be in current working directory
    
    cat('Calculating the second pass time...\n')
    
    FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
    Sub = c()
    Item = c()
    Cond = c()
    SecondFT2 = c()
    SecondFT3 = c()
    SecondFT4 = c()
    subindex = unique(FTtotalASRptReg$sub)
    for(i in subindex)
    {
      tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
      itemindex = unique(tempsub$item0)
      for(j in itemindex)
      {
        tempitem = tempsub[tempsub$item0 %in% j,]
        Sub=c(Sub,i)
        Item = c(Item,j)
        Cond = c(Cond, unique(tempitem$cond0)[[1]])
        tempsecFT = tempitem[tempitem$passtimes %in% 2,]
        if(is.na(tempsecFT$FFT[1]))
        {
          SecondFT2 = c(SecondFT2,0)
        }
        else
        {
          SecondFT2 = c(SecondFT2, sum(tempsecFT$FFT))
        }
        
        tempsecFT = tempitem[tempitem$passtimes %in% 3,]
        if(is.na(tempsecFT$FFT[1]))
        {
          SecondFT3 = c(SecondFT3,0)
        }
        else
        {
          SecondFT3 = c(SecondFT3, sum(tempsecFT$FFT))
        }
        
        tempsecFT = tempitem[tempitem$passtimes %in% 4,]
        if(is.na(tempsecFT$FFT[1]))
        {
          SecondFT4 = c(SecondFT4,0)
        }
        else
        {
          SecondFT4 = c(SecondFT4, sum(tempsecFT$FFT))
        }
      }
      cat(i,' has been done!!!','\n')
    }
    ROIsecondFT = data.frame(Sub, Item, Cond, SecondFT2, SecondFT3, SecondFT4, stringsAsFactors = F)
    write.csv(ROIsecondFT, paste(outputdir,'ROIsecondFT.csv', sep = '/'), row.names = F, quote = F)
    cat('ROI second fixation duration has been done','\n\n')
    
  }

###### skip rate
funSkipRate <- function(outputdir)
{
  cat('Calculating the skip rate...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)

  FTtotalASRptReg %>% 
    filter(ROI0 == T) %>% 
    group_by(sub0, cond0, item0) %>% 
    summarise(ffd0Mean = mean(ffd0)) %>% 
    mutate(Sub = sub0,
           Cond = cond0,
           Item = item0,
           skiprate = ifelse(ffd0Mean > 0, 1,0)) %>% 
    select(-sub0, -cond0, -item0, -ffdMean) %>% 
    write_csv('ROISkipRate.csv')
  cat('Skip Rate is done!!!\n\n')
}

###### integrate
funIntegrate <-
  function(workdir = getwd(),
           outputdir = workdir,
           FDMax, FDMin,
           ROIfilename1,
           DA1_to_csv = T,
           preprocess = T,
           TTFFD = T,
           FTnum = T,
           GazeDuration = T,
           Regression = T,
           SaccadeLength = T,
           SecondPassTime = T,
           FixationProportion = T,
           SkipRate = T,
           DataIntegrate = T)
  {
    # integrate the DA1 converting to csv, preprocessing and measures extracting
    # rio package is need, but dont worry about it because this function will check whether this package has been downloaded and will download it if not.
    # workdir - a character string indicating where the files you wang to use first are, defaultly the current working directory
    # outputdir - a character string indicating where you want to put out all the preprocessing and result files, can be a vector if you have more than one ROI
    # FDMax - numeric indicating the longest duration of fixation point you want to remain
    # FDMin - numeric indicating the shortest duration of fixation point you want to remain
    # ROIfilename1 - a character string indicating the name of the ROI location file (can be a vector if you have more than one ROI)
    # DA1_to_csv - logical indicating whether to convert the DA1 files
    # preprocess - logical indicating whether to preprocess
    # TTFFD - logical indicating whether to calculating total time and first fixation duration in ROI
    # FTnum - logical indicating whether to calculating the fixation numbers and its proportion in ROI
    # GazeDuration - logical indicating whether to calculating the gaze duration through ROI
    # Regression - logical indicating whether to calculating data regarding regression in ROI
    # SaccadeLength - logical indicating whether to calculating the saccade length to and from ROI
    # SecondPassTime - logical indicating whether to calculating the second pass time through ROI
    # FixationProportion - logical indicating whether to calculating the fixation proportion when passing ROI first time
    # DataIntegrate - logical whether to integrate all the result files into one result file
    
    # check whether the rio package has been downloaded
    if(sum(unique(installed.packages()[,c('Package')] %in% 'rio')) == 0)
    {install.packages('rio')}
    library(rio)
    if(DA1_to_csv == T)
    {
      funDA12csv()
    }
    if(preprocess == T)
    {
      for(i in outputdir)
      {
        funpreprocess(workdir, outputdir = i, FDMax = FDMax, FDMin = FDMin, ROIfilename = ROIfilename1[which(outputdir %in% i)])
      }
    }
    checkpreprocess = dir(pattern = 'FTtotalASRptReg.csv')
    if(length(checkpreprocess) == 1)
    {
      for(i in outputdir)
      {
        if(TTFFD == T)
        {
          funROITTFFD(i)
          #cat('Total time and First fixation duration is done!\n\n')
          #print(i)
          
        }
        if(FTnum == T)
        {
          funFTROInum(i)
          #cat('Number of fixation point in ROI is done!\n\n')
          #print(i)
        }
        if(GazeDuration == T)
        {
          funROIgazeduration(i)
          #cat('Gaze duration is done\n\n')
          #print(i)
        }
        if(Regression == T)
        {
          funROIregressionin(i)
          funROIregressionout(i)
          #cat('Regression on ROI is done\n\n')
          #print(i)
        }
        if(SaccadeLength == T)
        {
          funROIsaccadelength(i)
          #cat('Saccade Length is done\n\n')
          #print(i)
        }
        if(SecondPassTime == T)
        {
          funROIsecondFT(i)
          #cat('Second Pass Time is done\n\n')
          #print(i)
        }
        if(FixationProportion == T)
        {
          funROIfixationprop(i)
          #cat('Fixation Proportion is done!\n\n')
          #print(i)
        }
        if(SkipRate == T)
        {
          funSkipRate(i)
        }
        if(DataIntegrate == T)
        {
          
          setwd(i)
          datafilename = dir(pattern = 'ROI[a-zA-Z]')
          datafilename = datafilename[grep('.csv', datafilename)]
          Totalposition = grep('ROI[Tt]otal.csv',datafilename)
          if(length(Totalposition) > 0)
          {datafilename = datafilename[-1*Totalposition]}
          if(length(datafilename) == 0)
          {cat('Error: there is no data file!')}
          if(length(datafilename) == 1)
          {cat('There is only one file, so there is no need to integrate')}
          if(length(datafilename) > 1)
          {
            ROItotal = import(datafilename[1])
            for (ii in 2:length(datafilename)) {
              ROItotal = merge(ROItotal,import(datafilename[ii]), by = c('Sub','Item','Cond'))
            }
            export(ROItotal, paste(i, 'ROITotal.csv', sep = '/'))
            cat('Data Integrate is done!\n\n')
            
          }
        }
        cat('Congratulations!!!\n\n')
      }
    }else
    {cat('\nError:You have not done the preprocess analysis, please be sure that you have done that!\n\n')}
  }

###### GUI
funGUI <-
  function()
  {
    if(sum(unique(installed.packages()[,c('Package')] %in% 'tools')) == 0)
    {install.packages('tools')}
    if(sum(unique(installed.packages()[,c('Package')] %in% 'fgui')) == 0)
    {install.packages('fgui')}
    library(fgui)
    res = gui(funIntegrate,
              argOption = list(DA1_to_csv = c('T','F'), preprocess = c('T','F'), TTFFD = c('T','F'),
                               FTnum = c('T','F'), GazeDuration = c('T','F'),
                               Regression = c('T','F'), SaccadeLength = c('T','F'),
                               SecondPassTime = c('T','F'), FixationProportion = c('T','F'),
                               SkipRate = c('T', 'F'),
                               DataIntegrate = c('T','F')),
              #argEdit = list(FDMax = NULL, FDMin = NULL),
              title = 'DPEEM')
  }
