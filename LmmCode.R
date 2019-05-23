LmmCode = function(df=NULL, DV=NULL, IV=NULL, Cluster=NULL, output = '', Ifrun = F, Ifanova = F,
                   Manual = F, Manualcode = NULL){
  library(tidyverse)
  if(!isTRUE(Manual)){
    IVL = length(IV)
    IVComB = c()
    for (ll in 1:IVL) {
      ComB = combn(IV,ll) %>% t()
      if(ll == 1){IVComB = c(IVComB, ComB[,1])}else{
        for (rr in 1:nrow(ComB)) {
          IVComB = c(IVComB, paste(ComB[rr,], collapse = ':'))
        }
      }
    }

    if(IVL == 3){
      RSlope = paste(IVComB, collapse = ' + ')
      RSlope = c(RSlope, paste(IVComB[-7], collapse = ' + '))
      for (nn in 4:6) {
        RSlope = c(RSlope, paste(IVComB[-c(nn,7)], collapse = ' + '))
      }

      for (nn in 4:6) {
        RSlope = c(RSlope, paste(IVComB[c(1:3,nn)], collapse = ' + '))
      }

      RSlope = c(RSlope, paste(IVComB[1:3], collapse = ' + '))

      for (nn in 1:3) {
        RSlope = c(RSlope, paste(IVComB[-c(nn,4:7)],collapse = ' + '))
      }

      for (nn in 1:3) {
        RSlope = c(RSlope, paste(IVComB[nn],collapse = ' + '))
      }

      RSlope = paste(' + ',RSlope,'')
      RSlope = c(RSlope, '')

      Mcode = c()
      for (ni in 1:length(RSlope)) {
        for (ns in 1:length(RSlope)) {
          Mcode = c(Mcode,
                paste0('M.I',ni,'.S',ns,' = lmer(data = ',df, ', ',DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                       '(1',RSlope[ns],'|',Cluster[1],') + ',
                       '(1',RSlope[ni],'|',Cluster[2],'))'))
        }
        Mcode = c(Mcode,
                  paste0('M.I',ni,'.S',length(RSlope)+1,' = lmer(data = ',df, ', ',DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ni],'|',Cluster[2],'))'))
      }

      for (ns in 1:length(RSlope)) {
        Mcode = c(Mcode,
                  paste0('M.I',length(RSlope)+1,'.S',ns,' = lmer(data = ',df, ', ',DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ns],'|',Cluster[1],'))'))
      }
    }

    if(IVL == 2){
      RSlope = paste(IVComB, collapse = ' + ')
      RSlope = c(RSlope, paste(IVComB[-3], collapse = ' + '))
      for (nn in 1:2) {
        RSlope = c(RSlope, paste(IVComB[-c(nn,3)], collapse = ' + '))
      }

      RSlope = paste(' + ',RSlope,'')
      RSlope = c(RSlope, '')

      Mcode = c()
      for (ni in 1:length(RSlope)) {
        for (ns in 1:length(RSlope)) {
          Mcode = c(Mcode,
                    paste0('M.I',ni,'.S',ns,' = lmer(data = ',df, ', ',DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                           '(1',RSlope[ns],'|',Cluster[1],') + ',
                           '(1',RSlope[ni],'|',Cluster[2],'))'))
        }
        Mcode = c(Mcode,
                  paste0('M.I',ni,'.S',length(RSlope)+1,' = lmer(data = ',df, ', ',DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ni],'|',Cluster[2],'))'))
      }

      for (ns in 1:length(RSlope)) {
        Mcode = c(Mcode,
                  paste0('M.I',length(RSlope)+1,'.S',ns,' = lmer(data = ',df, ', ',DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ns],'|',Cluster[1],'))'))
      }
    }

    rio::export(as.data.frame(Mcode), paste('Mcode',output,'.txt'))
  }

  if(isTRUE(Manual) & !is.null(Manualcode)){
    Mcode = rio::import(Manualcode, sep = '\t')[[1]]
  }

  if(isTRUE(Ifrun)){
    cat('\n################################################################\n\nModel running...\n\n')
    for (mm in Mcode) {
      tic = Sys.time()
      eval(parse(text = mm))
      cat(mm,' is done/n', Sys.time() - tic, '\n')
    }
    cat('Model running finished\n\n')
    if(isTRUE(Ifanova)){
      cat('\n################################################################\n\nModel comparison is doing...\n\n')
      a = ls()[grep(pattern = 'M.I[0-9]', ls())] %>% paste(.,collapse = ', ')
      aa = paste0('Anovatable_',output,' = anova(',a,')')
      tic = Sys.time()
      eval(parse(text = aa))
      aaa = paste0('Anovatable_',output)
      eval(parse(text = paste0(aaa,' %>% as_tibble(rownames = NA) %>% write_csv(',aaa,'.csv)')))
      cat('Model comparison is done\n\n')
      Sys.time() - tic
    }
  }
}
