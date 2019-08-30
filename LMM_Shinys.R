
rm(list = ls())
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(parallel)){install.packages('parallel')}
if(!require(shiny)){install.packages('shiny')}
if(!require(shinydashboard)){install.packages('shinydashboard')}
if(!require(interactions)){install.packages('interactions')}
if(!require(lmerTest)){install.packages('lmerTest')}
if(!require(emmeans)){install.packages('emmeans')}
if(!require(jtools)){install.packages('jtools')}
if(!require(ggthemes)){install.packages('ggthemes')}
if(!require(simr)){install.packages('simr')}
if(!require(ggbeeswarm)){install.packages('ggbeeswarm')}
if(!require(rio)){install.packages('rio')}
cat('Would you use the bruceR package? please enter 1 or 2\n',
    '1: Yes, I use\n',
    '2: No, I dont use.')
p = scan(nmax = 1)
while(p != 1 & p != 2){
  print('You have not input the right number, please enter again.')
  p = scan(nmax = 1)
}
if(p == 1){
  if(!require(devtools)){install.packages('devtools')}
  if(!require(bruceR)){devtools::install_github("psychbruce/bruceR")}
}


formula_generate = function(DV, IV, Cluster){
  library(tidyverse)
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

    Formula = c()
    for (ni in 1:length(RSlope)) {
      for (ns in 1:length(RSlope)) {
        Formula = c(Formula,
                    paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                           '(1',RSlope[ns],'|',Cluster[1],') + ',
                           '(1',RSlope[ni],'|',Cluster[2],')'))
      }
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ni],'|',Cluster[2],')'))
    }

    for (ns in 1:length(RSlope)) {
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ns],'|',Cluster[1],')'))
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

    Formula = c()
    for (ni in 1:length(RSlope)) {
      for (ns in 1:length(RSlope)) {
        Formula = c(Formula,
                    paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                           '(1',RSlope[ns],'|',Cluster[1],') + ',
                           '(1',RSlope[ni],'|',Cluster[2],')'))
      }
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ni],'|',Cluster[2],')'))
    }

    for (ns in 1:length(RSlope)) {
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ns],'|',Cluster[1],')'))
    }
  }
  # write_csv(tibble(Formula),paste0(DV, 'Formulas.csv'))
  return(Formula)
}

LMMRun_Parallel = function(df, DV=NULL, IV=NULL, Cluster=NULL, Ifrun = F, output = NULL,
                           Manual = F, Manualcodefilename = NULL, Ncore = 4, Family = NULL){

  library(lmerTest)
  library(tidyverse)

  if(!isTRUE(Manual)){
    Formulas = formula_generate(DV = DV, IV = IV, Cluster = Cluster)
  }else{
    Formulas = read_csv(Manualcodefilename) %>% .[[1]]
  }

  Model_RunOneCore = function(formula.id){

    library(lmerTest)
    if(is.null(Family)){
      M = lmer(data = df, as.formula(Formulas[formula.id]))
      MAIC = AIC(M)
      MBIC = BIC(M)
      MConverge = ifelse(length(M@optinfo$conv$lme4$messages[grep(pattern = 'Model failed to converge',
                                                                  x = M@optinfo$conv$lme4$message)]) > 0,
                         F,T)
      M.Singular = isSingular(M)
      try({
        R2.C = MuMIn::r.squaredGLMM(M)[[2]]
        R2.M = MuMIn::r.squaredGLMM(M)[[1]]
      },silent = F)
      resulttable = data.frame(formula = Formulas[formula.id],
                               Nchar = nchar(Formulas[formula.id]),
                               R2.M = ifelse(exists('R2.M'),R2.M,NA),
                               R2.C = ifelse(exists('R2.C'),R2.C,NA),
                               AIC = MAIC,
                               BIC = MBIC,
                               Converge = MConverge,
                               Singular = M.Singular)
      return(resulttable)
    }else{
      M = glmer(data = df, as.formula(Formulas[formula.id]), family = Family)
      MAIC = AIC(M)
      MBIC = BIC(M)
      M.Singular = isSingular(M)
      MConverge = ifelse(length(M@optinfo$conv$lme4$messages[grep(pattern = 'Model failed to converge',
                                                                  x = M@optinfo$conv$lme4$message)]) > 0,
                         F,T)
      try({
        R2.C = MuMIn::r.squaredGLMM(M)[[2]]
        R2.M = MuMIn::r.squaredGLMM(M)[[1]]
      },silent = F)
      resulttable = data.frame(formula = Formulas[formula.id],
                               Nchar = nchar(Formulas[formula.id]),
                               R2.M = ifelse(exists('R2.M'),R2.M,NA),
                               R2.C = ifelse(exists('R2.C'),R2.C,NA),
                               AIC = MAIC,
                               BIC = MBIC,
                               Converge = MConverge,
                               Singular = M.Singular)
      return(resulttable)
    }
  }

  if (isTRUE(Ifrun)) {
    tic = Sys.time()
    formula.ids = sample(1:length(Formulas), length(Formulas))
    cat(length(Formulas), 'LMM models are running with', Ncore, ' parallel cores..............\n\n')
    library(parallel)
    cl <- makeCluster(Ncore)
    clusterExport(cl, c('df','DV','IV','Cluster',
                        'Ifrun','Manual','Manualcodefilename',
                        'Family','Formulas'), envir = environment())
    Results.DF <- do.call('rbind',parLapply(cl,formula.ids, Model_RunOneCore))
    stopCluster(cl)
    write_csv(Results.DF,paste0('ModelInfo',output,'.csv'))
    print(Sys.time() - tic)
    return(Results.DF)
  }
}

LMMRun_Once = function(df, Formula, Family=NULL){
  library(lmerTest)
  if(is.null(Family)){
    M = lmer(data = df, Formula)
    MAIC = AIC(M)
    MBIC = BIC(M)
    M.Singular = isSingular(M)
    resulttable = data.frame(formula = Formula,
                             AIC = MAIC,
                             BIC = MBIC,
                             Singular = M.Singular)
    return(resulttable)
  }else{
    M = glmer(data = df, Formula, family = Family)
    MAIC = AIC(M)
    MBIC = BIC(M)
    M.Singular = isSingular(M)
    resulttable = data.frame(formula = Formula,
                             AIC = MAIC,
                             BIC = MBIC,
                             Singular = M.Singular)
    return(resulttable)
  }
}
ViolinRawdata = function(df,
                         IVNumber = 2,
                         DepenVar,
                         Modu1, Modu2, Pred,
                         Themes, Color,
                         Title, Xlab, Ylab, LegendM, LabelSize, PAlpha,PSize=1){
  Height = eval(parse(text = paste0('(max(df$',DepenVar,')-min(df$',DepenVar,'))*0.007')))
  eval(parse(text = paste0('p = ggplot(data = df, aes(x = ',Pred,',y = ',DepenVar,', color = ',Modu1,'))+',
                           'geom_violin(alpha = 0,position = position_dodge(1))+',
                           'geom_quasirandom(dodge.width = 1, alpha = PAlpha, size = PSize, bandwidth = 0.1)+',
                           'geom_tile(data = df %>% group_by(', Pred, ',', Modu1, ifelse(IVNumber == 2,')',
                                                                                         paste0(',',Modu2,')')),
                           ' %>% summarize(M = mean(',DepenVar,')),',
                           'aes(x = ',Pred,',y = M, fill = ',Modu1,'),height = Height, width = 0.5, position = position_dodge(1), show.legend = F)',
                           ifelse(IVNumber == 3,
                                  paste0('+facet_wrap(~',Modu2,')'),''),
                           switch(Color,
                                  'Set1' = '+scale_color_brewer(palette = \'Set1\')+scale_fill_brewer(palette = \'Set1\')',
                                  'Set2' = '+scale_color_brewer(palette = \'Set2\')+scale_fill_brewer(palette = \'Set2\')',
                                  'Set3' = '+scale_color_brewer(palette = \'Set3\')+scale_fill_brewer(palette = \'Set3\')',
                                  'Grey' = paste0('+scale_color_manual(values = c(',GreyBreaker(df = df,Modulator = Modu1),'))+scale_fill_manual(values = c(',GreyBreaker(df = df,Modulator = Modu1),'))')),
                           '+labs(y = \'',Ylab,'\', x = \'',Xlab,'\', title = \'',Title,'\', color = \'',LegendM,'\')',
                           ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSize+5,'),',
                           ' axis.title.x = element_text(size = ',LabelSize,
                           '), axis.title.y = element_text(size = ',LabelSize,
                           '), legend.text = element_text(size = ',LabelSize-5,
                           '), legend.title = element_text(size = ',LabelSize,
                           '), axis.text.y = element_text(size = ',LabelSize-5,
                           '), axis.text.x = element_text(size = ',LabelSize-5,'))')))
  eval(parse(text = paste0('p ',
                           switch(Themes,
                                  'origin' = '',
                                  'APA' = '+jtools::theme_apa()',
                                  'Solar' = '+ggthemes::theme_solarized()',
                                  'Wall Street Journal' = '+ggthemes::theme_wsj()',
                                  'Economist' = '+ggthemes::theme_economist()',
                                  'LibreOffice' = '+ggthemes::theme_calc()',
                                  'Google Docs' = '+ggthemes::theme_gdocs()',
                                  'Stata' = '+ggthemes::theme_stata()',
                                  'New Excel' = '+ggthemes::theme_excel_new()',
                                  'Ugly Excel(NEVER USE PLEASE)' = '+ggthemes::theme_excel()'))))
}
ViolinBox = function(df,
                     IVNumber = 2,
                     DepenVar,
                     Modu1, Modu2, Pred,
                     Themes, Color,
                     Title, Xlab, Ylab, LegendM, LabelSize){
  eval(parse(text = paste0('p = ggplot(data = df, aes(x = ',Pred,',y = ',DepenVar,', color = ',Modu1,'))+',
                           'geom_violin(alpha = 0,position = position_dodge(1))+',
                           'geom_boxplot(position = position_dodge(1),width = 0.25,show.legend = F)',
                           ifelse(IVNumber == 3,
                                  paste0('+facet_wrap(~',Modu2,')'),''),
                           switch(Color,
                                  'Set1' = '+scale_color_brewer(palette = \'Set1\')+scale_fill_brewer(palette = \'Set1\')',
                                  'Set2' = '+scale_color_brewer(palette = \'Set2\')+scale_fill_brewer(palette = \'Set2\')',
                                  'Set3' = '+scale_color_brewer(palette = \'Set3\')+scale_fill_brewer(palette = \'Set3\')',
                                  'Grey' = paste0('+scale_color_manual(values = c(',GreyBreaker(df = df,Modulator = Modu1),'))+scale_fill_manual(values = c(',GreyBreaker(df = df,Modulator = Modu1),'))')),
                           '+labs(y = \'',Ylab,'\', x = \'',Xlab,'\', title = \'',Title,'\', color = \'',LegendM,'\')',
                           ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSize+5,'),',
                           ' axis.title.x = element_text(size = ',LabelSize,
                           '), axis.title.y = element_text(size = ',LabelSize,
                           '), legend.text = element_text(size = ',LabelSize-5,
                           '), legend.title = element_text(size = ',LabelSize,
                           '), axis.text.y = element_text(size = ',LabelSize-5,
                           '), axis.text.x = element_text(size = ',LabelSize-5,'))')))
  eval(parse(text = paste0('p ',
                           switch(Themes,
                                  'origin' = '',
                                  'APA' = '+jtools::theme_apa()',
                                  'Solar' = '+ggthemes::theme_solarized()',
                                  'Wall Street Journal' = '+ggthemes::theme_wsj()',
                                  'Economist' = '+ggthemes::theme_economist()',
                                  'LibreOffice' = '+ggthemes::theme_calc()',
                                  'Google Docs' = '+ggthemes::theme_gdocs()',
                                  'Stata' = '+ggthemes::theme_stata()',
                                  'New Excel' = '+ggthemes::theme_excel_new()',
                                  'Ugly Excel(NEVER USE PLEASE)' = '+ggthemes::theme_excel()'))))
}
GreyBreaker = function(df,Modulator){
  Number = eval(parse(text = paste0('length(unique(df$',Modulator,'))')))
  Breaker = seq(from=0, to = 50,round((80-40)%/%(Number-1)))
  return(paste(paste0('\'grey',Breaker,'\''), collapse = ','))
}

Datafilter = function(df, DV, FilterO = T, FilterNA = T, Group=NULL, ZV = 3){
  if(isTRUE(FilterO)){
    eval(parse(text = paste0('df = df %>% filter(',DV,' != 0)')))
  }

  if(isTRUE(FilterNA)){
    eval(parse(text = paste0('df = df %>% filter(!is.na(',DV,'))')))
  }

  if(is.null(Group)){
    eval(parse(text = paste0('df = df %>% mutate(Zvalue = scale(',DV,')) %>% filter(abs(Zvalue) < ',ZV,') %>% select(-Zvalue)')))
  }else{
    eval(parse(text = paste0('df = df %>% group_by(',Group,') %>% mutate(Zvalue = scale(',DV,')) %>% filter(abs(Zvalue) < ',ZV,') %>% select(-Zvalue)')))
  }


  return(df)
}

PowerTable = function(df,formula, family, fixedeffect, subject, minsub, maxsub, steps, Ncore = 4, Nsim=100, RunOrigin = T){
  tic = Sys.time()
  NumP = c(eval(parse(text = paste0('length(unique(df','$',subject,'))'))),
           seq(from = minsub, to = maxsub, steps))
  eval(parse(text = paste0('M = ',ifelse(family == 'gaussian','lmer(','glmer('),
                           'data = df', ',',
                           formula,
                           ifelse(family == 'gaussian',')',
                                  paste0(', family = \'',family,'\')')))))
  if(isTRUE(RunOrigin)){
    eval(parse(text = paste0('PA = powerSim(M, fixed(\'',fixedeffect,'\'), nsim = ',Nsim,', alpha = 0.05)')))
    Number = NumP[1]
    Power = mean(PA$pval<0.05)*100
    ConfLow = binom.test(x = sum(PA$pval<0.05),n = length(PA$pval),p = 0.5)$conf.int[1]*100
    ConfUp = binom.test(x = sum(PA$pval<0.05),n = length(PA$pval),p = 0.5)$conf.int[2]*100
    FirstOne = data.frame(SubNumber = Number,
                          PowerValue = Power,
                          ConfUp,ConfLow)
  }

  PowerOne = function(ss){
    library(simr)
    eval(parse(text = paste0('M2 = extend(M, along = \'',subject,'\',n = ',NumP[ss],')')))
    eval(parse(text = paste0('PA = powerSim(M2, fixed(\'',fixedeffect,'\'), nsim = ',Nsim,', alpha = 0.05)')))
    Number = NumP[ss]
    Power = mean(PA$pval<0.05)*100
    ConfLow = binom.test(x = sum(PA$pval<0.05),n = length(PA$pval),p = 0.5)$conf.int[1]*100
    ConfUp = binom.test(x = sum(PA$pval<0.05),n = length(PA$pval),p = 0.5)$conf.int[2]*100
    print(paste('Sample size with ', NumP[ss], ' has been done!'))
    return(data.frame(SubNumber = Number,
                      PowerValue = Power,
                      ConfUp,ConfLow))
  }


  SS = sample(2:length(NumP),length(NumP)-1)
  cat(length(NumP)-1, 'Power calculating are running with', Ncore, ' parallel cores..............\n\n')
  library(parallel)
  cl <- makeCluster(Ncore)
  clusterExport(cl, c('subject','NumP','fixedeffect','M','df','formula','Nsim'), envir = environment())
  clusterEvalQ(cl,c('simr','tidyverse'))
  DF <- do.call('rbind',parLapply(cl,SS, PowerOne))
  stopCluster(cl)

  if(isTRUE(RunOrigin)){
    DF = bind_rows(FirstOne, DF)
  }

  DF = DF %>% arrange(SubNumber)
  a = Sys.time()-tic
  cat('Power running has taken',a[[1]],attributes(a)$unit,'\n\n')
  return(DF)
}

LMM_Shinys = function(){
  ui <- dashboardPage(
    dashboardHeader(title = "Shiny LMM Builder"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome!", tabName = "Welcome", icon = icon("Wel")),
        menuItem("Data Filter", tabName = "DataFilter", icon = icon("DF")),
        menuItem("Formula Generator", tabName = "Formulagenerator", icon = icon("FG")),
        menuItem("Model optimization", tabName = "ModelOptimize", icon = icon("MO")),
        menuItem("Mixed model builder", tabName = "ModelBuilder", icon = icon("MB")),
        menuItem("Power calculator", tabName = "Powercal", icon = icon("PC"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'Welcome',
                box(title = 'Brief Introduction:',
                    textOutput(outputId = 'ShinyIntroduction1'),
                    textOutput(outputId = 'ShinyIntroduction2'),
                    textOutput(outputId = 'ShinyIntroduction3'),
                    textOutput(outputId = 'ShinyIntroduction4'),
                    textOutput(outputId = 'ShinyIntroduction5'),
                    textOutput(outputId = 'ShinyIntroduction6'),
                    textOutput(outputId = 'ShinyIntroduction7'))),
########## First tab content
        tabItem(tabName = 'DataFilter',
                fluidRow(
                  column(width = 6,
                         box(title = 'Parameters',
                             collapsible = T,status = 'info',width = NULL,solidHeader = T,
                             fileInput("file1DataFilter", "Choose the File of your data",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",'.xls','.txt','.xlsx')
                             ),

                             numericInput("obsDataFilter", "Set the number of observations to view:", 6),

                             textInput('DVDataFilter','Input the dependent variable:',NULL),

                             checkboxInput('Filter0DataFilter','Whether to filter the data equal to 0',value = F),

                             checkboxInput('FilterNADataFilter','Whether to filter the NA data',value = T),

                             textInput('GroupDataFilter','Input the group to categorise (seperated by comma). If there is no subgroup, need not input.',NULL),

                             sliderInput('ZVDataFilter','Set the Z value to filter',min = 1, max = 5,step = 0.1, value = 3),

                             downloadButton("downloadDataDataFilter", "Download the filtered Data"))),
                  column(width = 6,
                         box(title = 'Data raw Summary',
                             width = NULL,status = 'primary',solidHeader = T,collapsible = T,
                             tableOutput("DataSummaryDataFilter"),
                             textOutput('OldLineDataFilter')),
                         box(title = 'Data filtered Summary',
                             width = NULL,status = 'primary',solidHeader = T,collapsible = T,
                             tableOutput('DataFilteredDataFilter'),
                             textOutput('NewLineDataFilter')))
                  )),
########## Second tab content
        tabItem(tabName = "Formulagenerator",
                fluidRow(
                  column(width = 6,
                         box(
                           title = "Parameters",
                           collapsible = T,status = 'info',width = NULL,solidHeader = T,
                           textInput('DVFormulaG','Input the dependent variable:','Y'),

                           selectInput('IVNumberFormulaG','Select the number of fixed factors',choices = c(2,3)),

                           textInput('IV1FormulaG','Input the 1st factor:', 'A'),

                           textInput('IV2FormulaG','Input the 2nd factor:', 'B'),

                           textInput('IV3FormulaG','Input the 3rd factor:', 'C'),

                           textInput('Cluster1FormulaG','Input the 1st cluster variable:', 'Sub'),

                           textInput('Cluster2FormulaG','Input the 2nd cluster variable:', 'Item'),

                           downloadButton("downloadDataFormulaG", "Download the formulas")
                         )),
                  column(width = 6,
                         box(title = 'Formula',
                             collapsible = T,status = 'primary',width = NULL,solidHeader = T,
                             tableOutput("contentsFormulaG"))))
        ),

########## Third tab content
        tabItem(tabName = "ModelOptimize",
                fluidRow(
                  column(width = 6,
                         box(title = 'Parameters',
                             collapsible = T,status = 'info',width = NULL,solidHeader = T,
                             fileInput("file1ModelOptimize", "Choose the File of your data",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",'.xls','.txt','.xlsx')
                             ),
                             numericInput("obsModelOptimize", "Set the number of observations to view:", 6),

                             textInput('DVModelOptimize','Input the dependent variable:','Y'),

                             selectInput('IVNumberModelOptimize','Select the number of fixed factors',choices = c(2,3)),

                             textInput('IV1ModelOptimize','Input the 1st factor:', 'A'),

                             textInput('IV2ModelOptimize','Input the 2nd factor:', 'B'),

                             textInput('IV3ModelOptimize','Input the 3rd factor:', 'C'),

                             textInput('Cluster1ModelOptimize','Input the 1st cluster variable:', 'Sub'),

                             textInput('Cluster2ModelOptimize','Input the 2nd cluster variable:', 'Item'),

                             checkboxInput('IfrunModelOptimize','Whether to run the models',T),

                             checkboxInput('ManualModelOptimize','Whether to run models based on existing formulas',F),

                             textInput('mfileModelOptimize', 'Input file name containing existing formulas:',NULL),

                             selectInput('FamilyModelOptimize', 'Select the distribution family of dependent variable:',
                                         choices = c('gaussian','binomial','poisson')),

                             sliderInput('NcoreModelOptimize','Set the number of parallel cores', min = 1, max = 20, value = 4, step = 1),

                             textInput('OutputModelOptimize', 'input the prefix name of ouput file:','Y'),

                             actionButton("RunModelOptimize", "Run!"))),

                  column(width = 6,
                         box(title = 'Raw data',
                             collapsible = T,status = 'primary',width = NULL,solidHeader = T,
                             tableOutput("RawModelOptimize"),
                             textOutput(outputId = 'EndModelOptimize')),
                         box(title = 'Model information',
                             collapsible = T,status = 'primary',width = NULL,solidHeader = T,
                             tableOutput("ModelInfomation")
                             ))

                )
        ),
########## Fourth tab content
        tabItem(tabName = 'ModelBuilder',
                fluidRow(column(width = 6,
                                box(title = 'Model Building Part:',
                                    collapsible = T,status = 'info',width = NULL,solidHeader = T,
                                    fileInput("file1ModelBuild", "Choose the File of your data",
                                              accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv",'.xls','.txt','.xlsx')
                                    ),
                                    numericInput("obsModelBuild", "Set the number of observations to view:", 6),

                                    selectInput('HLMModelBuild','Whether to perform the HLM or GLM?',choices = c('HLM','GLM')),
                                    textInput('FormulaModelBuild','Input the formula:',NULL),

                                    selectInput('FamilyModelBuild', 'Select the distribution family of dependent variable:',
                                                choices = c('gaussian','binomial','poisson')),

                                    selectInput('ContrastsModelBuild','Select the type of contrasts:',
                                                choices = c('sum','treatment'))),
                                box(title = 'Histogram on each Participants:',
                                    collapsible = T,status = 'info',collapsed = T,width = NULL,solidHeader = T,
                                    checkboxInput('Split.SubModelBuild','Whether to plot histogram based on each subject?',F),
                                    selectInput('TransferModelBuild','Select type of data transfer',
                                                choices = c('Origin', 'Log E', 'Log 10', 'Minus Reverse', 'Minus Reverse Multi 1000')),
                                    sliderInput('NumColModelBuild','How many columns should the histogram be arranged?',min = 1, max = 20,step = 1,value = 3),
                                    textInput('DepenVarModelBuild','Input the name of column indication dependent variable',NULL),
                                    textInput('SubNameModelBuild','Input the name of column indicating subject',NULL)),

                                box(title = 'Summary and Anova download:',
                                    collapsible = T,status = 'info',collapsed = F,width = NULL,solidHeader = T,
                                    downloadButton("downloadSummaryModelBuild", "Download the Summary table"),
                                    downloadButton("downloadAnovaModelBuild", "Download the Anova table")),

                                box(title = 'Simple effect analysis:',
                                    collapsible = T,status = 'info',collapsed = T,width = NULL,solidHeader = T,
                                    checkboxInput('SimpleEffectModelBuild',label = 'Whether to preform the simple effect analysis?',value = F),
                                    selectInput('IVNumberModelBuild','Select the number of fixed factors',choices = c(2,3)),
                                    numericInput('pbkrlimitModelBuild','Set the limit of pbkrtest:',value = 3000),
                                    textInput('PredictorModelBuild','Input the predictor`s name',NULL),
                                    textInput('Modulator1ModelBuild','Input the 1st modulator`s name',NULL),
                                    textInput('Modulator2ModelBuild','Input the 2nd modulator`s name if have',NULL),

                                    downloadButton("downloadEmmeansModelBuild", "Download the Emmeans table"),
                                    downloadButton("downloadComparisonModelBuild", "Download the Comparison table")),

                                box(title = 'Set the parameters to plot:',
                                    collapsible = T,status = 'info',collapsed = T,width = NULL,solidHeader = T,
                                    helpText('NOTE! Plot is based on the parameter you set in Simple effect analysis'),
                                    checkboxInput('PlotModelBuild','Whether to plot',F),
                                    selectInput('GeomtypeModelBuild','Select the geometry to draw',
                                                choices = c('bar','line','violin plus raw data','violin plus boxplot')),
                                    textInput('DepenVar2ModelBuild','Input the name of column indication dependent variable to plot violin',NULL),
                                    numericInput('PAlphaModelBuild','Set the point alpha for Violin plot',value = 0.2, min = 0,max = 1),
                                    numericInput('PSizeModelBuild','Set the point Size for Violin plot',value = 1,step = 0.1),
                                    selectInput('ThemesModelBuild','Select the theme of plot:',
                                                choices = c('origin','APA','Solar','Wall Street Journal',
                                                            'Economist','LibreOffice',
                                                            'Google Docs','Stata',
                                                            'New Excel','Ugly Excel(NEVER USE PLEASE)')),
                                    selectInput('ColorModelBuild','Select the color palette',
                                                choices = c('Set1','Set2','Set3','Grey')),
                                    textInput('TitleModelBuild','Input the title of plot:',NULL),
                                    textInput('YlabModelBuild','Input the label of y axis:', NULL),
                                    textInput('XlabModelBuild','Input the label of x axis:', NULL),
                                    textInput('LegendMModelBuild','Input the title of legend:', NULL),
                                    sliderInput(inputId = 'LabelSizeModelBuild',label = 'Set the size of plot labels and title',min = 10, max = 50,step = 1, value = 10),
                                    selectInput(inputId = 'FontFamilyModelBuild',label = 'Set the family of font in plot',
                                                choices = c('Default','Times New Roman','SimSun','Cambria','Times','Arial','Calibri'),selected = 'Default'),
                                    checkboxInput('DotsModelBuild','Whether draw raw data (dots)?',F),
                                    numericInput(inputId = 'WidthModelBuild',label = 'Set the plot Width',value = 400, min = 400, max = 10000,step = 1),
                                    numericInput(inputId = 'HeightModelBuild',label = 'Set the plot Height',value = 400, min = 400, max = 10000,step = 1))),
                         column(width = 6,
                                box(title = 'Data Summary',
                                    width = NULL,status = 'primary',solidHeader = T,collapsible = T,
                                               tableOutput("DataSummaryModelBuild"),textOutput('InformationModelBuild')),
                                box(title = 'Histogram of Subjects',
                                    width = NULL,status = 'primary',solidHeader = T,collapsible = T,collapsed = T,
                                    plotOutput('Sub.PlotModelBuild',inline = T)),
                                box(title = 'Model Information',
                                    width = NULL,status = 'primary',solidHeader = T,collapsible = T,
                                    tabsetPanel(type = 'tabs',
                                                tabPanel('Model Summary',verbatimTextOutput("summaryModelBuild")),
                                                tabPanel('BruceR Model Summary',verbatimTextOutput("summary2ModelBuild")),
                                                tabPanel('Anova',tableOutput("AnovaModelBuild")),
                                                tabPanel('Residual Distribution', plotOutput('ResiPlotModelBuild',inline = F)),
                                                tabPanel('Residual Density', plotOutput('ResiPlot2ModelBuild',inline = F)))),
                                box(title = 'Simple Effect',
                                    width = NULL,status = 'primary',solidHeader = T,collapsible = T,collapsed = T,
                                    tabsetPanel(type = 'tabs',
                                                tabPanel('Descriptive information',tableOutput('EmmeansModelBuild')),
                                                tabPanel('Comparison',tableOutput('ComparisonModelBuild')))),
                                box(title = 'Plot',
                                    width = NULL,status = 'primary',solidHeader = T,collapsible = T,collapsed = T,
                                    plotOutput('PlotModelBuild',inline = T))))),
########## Fifth tab content
        tabItem(tabName = 'Powercal',
                fluidRow(
                  column(width = 6,
                         box(title = 'Parameters',
                             collapsible = T,status = 'info',width = NULL,solidHeader = T,
                             actionButton(inputId = 'RunPowercal',label = 'Run Power Calculation !!'),

                             fileInput("file1Powercal", "Choose the File of your data",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",'.xls','.txt','.xlsx')
                             ),
                             numericInput("obsPowercal", "Set the number of observations to view:", 6),
                             textInput('FormulaPowercal','Input the formula:',NULL),

                             checkboxInput(inputId = 'RunOriginPowercal',label = 'Whether run the origin model?',value = T),

                             selectInput('FamilyPowercal', 'Select the distribution family of dependent variable:',
                                         choices = c('gaussian','binomial','poisson')),

                             textInput('FixedEffectPowercal','Input the fixed effect to examine:',NULL),

                             textInput('SubjectPowercal','Input the name of variable indicating participant/item:',NULL),

                             numericInput('MinSubPowercal','Input the minimum number of participants/item to check:',20),
                             numericInput('MaxSubPowercal','Input the maximum number of participants/item to check:',80),

                             sliderInput('StepPowercal','Set the step to increase:',min = 1, max = 20,step = 1,value = 10),

                             numericInput('NsimPowercal','Input the number of simulations:',100),

                             sliderInput('NcorePowercal','Set the paralle cores to run',min = 1, max = 20, step = 1, value = 4),

                             downloadButton("downloadDataPowercal", "Download the power table"))),
                  column(width = 6,
                         box(title = 'Data Summary',
                             collapsible = T,status = 'primary',width = NULL,solidHeader = T,
                             tableOutput("DataSummaryPowercal")),
                         box(title = 'Power table',
                             collapsible = T,status = 'primary',width = NULL,solidHeader = T,
                             tableOutput("PowerPowercal")),
                         box(title = 'Plot',
                             collapsible = T,status = 'primary',width = NULL,solidHeader = T,
                             plotOutput('PlotPowercal')))
                         ))
      )
    )
  )

  server <- function(input, output) {
    output$ShinyIntroduction1 = renderText({
      print('Thanks for you to use the current shiny!')
    })
    output$ShinyIntroduction2 = renderText({
      print('This shiny interface includes five table item.')
    })
    output$ShinyIntroduction3 = renderText({
      print('The 1st is used to filter data.')
    })
    output$ShinyIntroduction4 = renderText({
      print('The 2nd is used to generate all possible formulas, which can assist the \'Model Optimizer\'.')
    })
    output$ShinyIntroduction5 = renderText({
      print('The 3rd is used to select the best model.')
    })
    output$ShinyIntroduction6 = renderText({
      print('The 4th is used to build one specific model.')
    })
    output$ShinyIntroduction7 = renderText({
      print('The last one is used to calculate the power.')
    })

    DVDataFilter = reactive(input$DVDataFilter)
    Filter0DataFilter = reactive(input$Filter0DataFilter)
    FilterNADataFilter = reactive(input$FilterNADataFilter)
    GroupDataFilter = reactive(input$GroupDataFilter)
    ZVDataFilter = reactive(input$ZVDataFilter)
    obsDataFilter = reactive(input$obsDataFilter)
    dfDataFilter = reactive({
      inFileDataFilter <- input$file1DataFilter

      if (is.null(inFileDataFilter))
        return(NULL)

      rio::import(inFileDataFilter$datapath)
    })
    df2DataFilter = reactive({
      Datafilter(df = dfDataFilter(),
                 DV = DVDataFilter(),
                 FilterO = Filter0DataFilter(),
                 FilterNA = FilterNADataFilter(),
                 Group = GroupDataFilter(),
                 ZV = ZVDataFilter())
    })

    output$DataSummaryDataFilter = renderTable({
      head(dfDataFilter(),n = obsDataFilter())
    })

    output$OldLineDataFilter = renderText({
      print(paste0('There are ', nrow(dfDataFilter()),' lines.'))
    })

    output$DataFilteredDataFilter = renderTable({
      head(df2DataFilter(), n = obsDataFilter())
    })

    output$NewLineDataFilter = renderText({
      print(paste0('There are ', nrow(df2DataFilter()),' lines.'))
    })

    output$downloadDataDataFilter <- downloadHandler(
      filename = function() {
        paste(input$DVDataFilter,'Filtered', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(df2DataFilter(), file)
      }
    )

    DVFormulaG = reactive(input$DVFormulaG)

    IVNumberFormulaG = reactive(input$IVNumberFormulaG)

    IV1FormulaG = reactive(input$IV1FormulaG)

    IV2FormulaG = reactive(input$IV2FormulaG)

    IV3FormulaG = reactive(input$IV3FormulaG)

    Cluster1FormulaG = reactive(input$Cluster1FormulaG)

    Cluster2FormulaG = reactive(input$Cluster2FormulaG)

    FormulasFormulaG = reactive({
      if (IVNumberFormulaG() == 2) {
        formula_generate(DV = DVFormulaG(),
                         IV = c(IV1FormulaG(), IV2FormulaG()),
                         Cluster = c(Cluster1FormulaG(), Cluster2FormulaG()))
      }else{
        formula_generate(DV = DVFormulaG(),
                         IV = c(IV1FormulaG(), IV2FormulaG(),IV3FormulaG()),
                         Cluster = c(Cluster1FormulaG(), Cluster2FormulaG()))
      }
    })


    output$contentsFormulaG <- renderTable({
      FormulasFormulaG() %>% as_tibble()
    })

    output$downloadDataFormulaG <- downloadHandler(
      filename = function() {
        paste(input$DVFormulaG,'Formulas', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(FormulasFormulaG() %>% as_tibble(), file)
      }
    )

    obsModelOptimize = reactive(input$obsModelOptimize)
    DVModelOptimize = reactive(input$DVModelOptimize)

    IVNumberModelOptimize = reactive(input$IVNumberModelOptimize)

    IV1ModelOptimize = reactive(input$IV1ModelOptimize)

    IV2ModelOptimize = reactive(input$IV2ModelOptimize)

    IV3ModelOptimize = reactive(input$IV3ModelOptimize)

    Cluster1ModelOptimize = reactive(input$Cluster1ModelOptimize)

    Cluster2ModelOptimize = reactive(input$Cluster2ModelOptimize)

    ManualModelOptimize = reactive(input$ManualModelOptimize)
    mfileModelOptimize = reactive(input$mfileModelOptimize)

    IfrunModelOptimize = reactive(input$IfrunModelOptimize)

    FamilyDModelOptimize = reactive({
      switch(input$FamilyModelOptimize,
             "gaussian" = NULL,
             "binomial" = 'binomial',
             "poisson" = 'poisson')
    })


    OutputModelOptimize = reactive(input$OutputModelOptimize)

    dfModelOptimize = reactive({
      mfileModelOptimize = reactive(input$mfileModelOptimize)

      if(is.null(mfileModelOptimize)){
        m = NULL
      }else{m = mfileModelOptimize()}

      inFileModelOptimize <- input$file1ModelOptimize

      if (is.null(inFileModelOptimize))
        return(NULL)

      rio::import(inFileModelOptimize$datapath)
    })

    ModelInfo = eventReactive(input$RunModelOptimize,{
      if(IVNumberModelOptimize() == 2){
        LMMRun_Parallel(df = dfModelOptimize(),
                        DV = DVModelOptimize(),
                        IV = c(IV1ModelOptimize(), IV2ModelOptimize()),
                        Cluster = c(Cluster1ModelOptimize(), Cluster2ModelOptimize()),
                        Manual = ManualModelOptimize(),Manualcodefilename = mfileModelOptimize(), Family = FamilyDModelOptimize(),
                        Ifrun = IfrunModelOptimize(),
                        Ncore = input$NcoreModelOptimize,
                        output = OutputModelOptimize())  %>% arrange(Singular,-Converge,-Nchar) %>%
          select(-Nchar)
      }else{
        LMMRun_Parallel(df = dfModelOptimize(),
                        DV = DVModelOptimize(),
                        IV = c(IV1ModelOptimize(), IV2ModelOptimize(),IV3ModelOptimize()),
                        Cluster = c(Cluster1ModelOptimize(), Cluster2ModelOptimize()),
                        Manual = ManualModelOptimize(),Manualcodefilename = mfileModelOptimize(), Family = FamilyDModelOptimize(),
                        Ifrun = IfrunModelOptimize(),
                        Ncore = input$NcoreModelOptimize,
                        output = OutputModelOptimize()) %>% arrange(Singular,-Converge,-Nchar) %>%
          select(-Nchar)
      }
    },ignoreNULL = T)

    output$RawModelOptimize = renderTable({
      head(dfModelOptimize(),obsModelOptimize())
    })

    output$ModelInfomation <- renderTable({
      ModelInfo()
    })

    output$EndModelOptimize = eventReactive(input$Run,{
      if(OutputModelOptimize() %in% c('Marry','Thank', 'Wey')){
        'Thank help from Wey. Would you marry me?'
      }else{'   '}
    },ignoreNULL = F)

    HLMModelBuild = reactive(input$HLMModelBuild)
    FormulaModelBuild = reactive(input$FormulaModelBuild)

    FamilyModelBuild = reactive(input$FamilyModelBuild)

    obsModelBuild = reactive(input$obsModelBuild)

    Split.SubModelBuild = reactive(input$Split.SubModelBuild)
    TransferModelBuild = reactive(input$TransferModelBuild)
    NumColModelBuild = reactive(input$NumColModelBuild)
    SubNameModelBuild = reactive(input$SubNameModelBuild)
    DepenVarModelBuild = reactive(input$DepenVarModelBuild)
    WidthModelBuild = reactive(input$WidthModelBuild)
    HeightModelBuild = reactive(input$HeightModelBuild)

    SimpleEffectModelBuild = reactive(input$SimpleEffectModelBuild)
    IVNumberModelBuild = reactive(input$IVNumberModelBuild)
    pbkrlimitModelBuild = reactive(input$pbkrlimitModelBuild)
    PredictorModelBuild = reactive(input$PredictorModelBuild)
    Modulator1ModelBuild = reactive(input$Modulator1ModelBuild)
    Modulator2ModelBuild = reactive(input$Modulator2ModelBuild)

    PLOTModelBuild = reactive(input$PlotModelBuild)
    GeomtypeModelBuild = reactive(input$GeomtypeModelBuild)
    DepenVar2ModelBuild = reactive(input$DepenVar2ModelBuild)
    PAlphaModelBuild = reactive(input$PAlphaModelBuild)
    PSizeModelBuild = reactive(input$PSizeModelBuild)
    ThemesModelBuild = reactive(input$ThemesModelBuild)
    ColorModelBuild = reactive(input$ColorModelBuild)
    DotsModelBuild = reactive(input$DotsModelBuild)
    TitleModelBuild = reactive(input$TitleModelBuild)
    YlabModelBuild = reactive(input$YlabModelBuild)
    XlabModelBuild = reactive(input$XlabModelBuild)
    LegendMModelBuild = reactive(input$LegendMModelBuild)
    LabelSizeModelBuild = reactive(input$LabelSizeModelBuild)
    FontFamilyModelBuild = reactive(input$FontFamilyModelBuild)

    ContrastsModelBuild = reactive({
      switch(input$ContrastsModelBuild,
             'sum' = 'contr.sum',
             'treatment' = 'contr.treatment')
    })

    dfModelBuild = reactive({
      inFileModelBuild <- input$file1ModelBuild

      if (is.null(inFileModelBuild))
        return(NULL)

      rio::import(inFileModelBuild$datapath)
    })

    MModelBuild = reactive({
      options(contrasts = c(ContrastsModelBuild(),'contr.poly'))
      #DF = df()
      if(FamilyModelBuild() %in% 'gaussian'){
        eval(parse(text = paste0(ifelse(HLMModelBuild() %in% 'HLM', 'lmer','lm'),
                                 '(data = dfModelBuild(),formula = ',FormulaModelBuild(),')')))

      }else{
        eval(parse(text = paste0(ifelse(HLMModelBuild() %in% 'HLM', 'glmer','glm'),
                                 '(data = dfModelBuild(),formula = ',FormulaModelBuild(),',family = ', FamilyModelBuild(),')')))
      }
    })

    output$DataSummaryModelBuild = renderTable({

      head(dfModelBuild(),n = obsModelBuild())
    })

    output$summaryModelBuild = renderPrint({
      summary(MModelBuild()) %>% print()
      cat('\n  If you use sum contrasts, be aware the \'Estimate\' might not be equal to the real differences between the levels of the factor.',
          '\n  For this please see https://zhuanlan.zhihu.com/p/76459927',
          '\n  If you want to get the real differences, we suggest you use the simple effect analysis block below by setting the predictor and the first modulator as the same factor while keeping the number of fixed factor as 2')
    })
    output$summary2ModelBuild = renderPrint({
      if(HLMModelBuild() %in% 'HLM'){
        if(FamilyModelBuild() %in% 'gaussian'){
          HLM_summary(MModelBuild())
        }else{
          HLM_summary(formula = FormulaModelBuild() %>% as.formula(),
                      data = dfModelBuild(),
                      family = FamilyModelBuild())
        }
      }else{
        GLM_summary(MModelBuild())
      }

      cat('\n  If you use sum contrasts, be aware the \'Estimate\' might not be equal to the real differences between the levels of the factor.',
          '\n  For this please see https://zhuanlan.zhihu.com/p/76459927',
          '\n  If you want to get the real differences, we suggest you use the simple effect analysis block below by setting the predictor and the first modulator as the same factor while keeping the number of fixed factor as 2')
    })

    output$downloadSummaryModelBuild <- downloadHandler(
      filename = function() {
        paste('Fixed_Effect_Table', ".csv", sep = "")
      },
      content = function(file) {

        M1 = round(summary(MModelBuild())$coef,digits = 3)
        M1 = bind_cols(tibble(Effect = rownames(M1)),
                       as_tibble(M1))
        rio::export(M1, file)
      }
    )
    output$AnovaModelBuild = renderTable({
      M = anova(MModelBuild())
      bind_cols(tibble(Effect = rownames(M)),
                as_tibble(M))


    })

    output$downloadAnovaModelBuild <- downloadHandler(

      filename = function() {
        paste('Anova_Table', ".csv", sep = "")
      },
      content = function(file) {
        M1 = round(anova(MModelBuild()),digits = 3)
        M1 = bind_cols(tibble(Effect = rownames(M1)),
                       as_tibble(M1))
        rio::export(M1, file)
      }
    )

    output$ResiPlotModelBuild = renderPlot({
      qplot(residuals(MModelBuild()), ylab = 'Count', xlab = 'Residual', color = I('black'), fill = I('purple'))
    },width = function() return(WidthModelBuild()), height = function() return(HeightModelBuild()))

    output$ResiPlot2ModelBuild = renderPlot({
      qplot(residuals(MModelBuild()), geom = 'density', ylab = 'Propability Density', xlab = 'Residual', size=I(1), color = I('black'))+
        geom_vline(xintercept = 0, size=1, color = I('red'))
    },width = function() return(WidthModelBuild()), height = function() return(HeightModelBuild()))

    output$EmmeansModelBuild = renderTable({
      if(isTRUE(SimpleEffectModelBuild())){
        emm_options(pbkrtest.limit = pbkrlimitModelBuild())
        if(IVNumberModelBuild() == 2){
          eval(parse(text = paste0('emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),')$emm')))
        }else{
          eval(parse(text = paste0('emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),'|',Modulator2ModelBuild(),')$emm')))
        }
      }

    })

    output$downloadEmmeansModelBuild = downloadHandler(

      filename = function() {
        paste('Emmeans_Table', ".csv", sep = "")
      },
      content = function(file) {
        if(isTRUE(SimpleEffectModelBuild())){
          emm_options(pbkrtest.limit = pbkrlimitModelBuild())
          options(digits = 3)
          if(IVNumberModelBuild() == 2){
            eval(parse(text = paste0('M1 = emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),')$emm %>% as_tibble()')))
          }else{
            eval(parse(text = paste0('M1 = emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),'|',Modulator2ModelBuild(),')$emm %>% as_tibble()')))
          }

        }
        rio::export(M1, file)
      }
    )

    output$ComparisonModelBuild = renderTable({
      if(isTRUE(SimpleEffectModelBuild())){

        if(IVNumberModelBuild() == 2){
          eval(parse(text = paste0('emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),')$contr %>% as_tibble()')))
        }else{
          eval(parse(text = paste0('emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),'|',Modulator2ModelBuild(),')$contr %>% as_tibble()')))
        }

      }
    })

    output$downloadComparisonModelBuild = downloadHandler(

      filename = function() {
        paste('Comparison_Table', ".csv", sep = "")
      },
      content = function(file) {
        if(isTRUE(SimpleEffectModelBuild())){

          options(digits = 3)
          if(IVNumberModelBuild() == 2){
            eval(parse(text = paste0('M1 = emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),')$contr %>% as_tibble()')))
          }else{
            eval(parse(text = paste0('M1 = emmeans(MModelBuild(), pairwise~',PredictorModelBuild(),'|',Modulator1ModelBuild(),'|',Modulator2ModelBuild(),')$contr %>% as_tibble()')))
          }

        }
        rio::export(M1, file)
      }
    )

    output$PlotModelBuild = renderPlot({
      if(isTRUE(PLOTModelBuild())){

        FontFamilySet = function(P,Family = 'Times'){
          p + theme(title = element_text(family = Family),
                    text = element_text(family = Family))
        }

        if(GeomtypeModelBuild() %in% 'violin plus raw data'){

          p = ViolinRawdata(df = dfModelBuild(),IVNumber = IVNumberModelBuild(),DepenVar = DepenVar2ModelBuild(),
                            Pred = PredictorModelBuild(),Modu1 = Modulator1ModelBuild(),Modu2 = Modulator2ModelBuild(),
                            Themes = ThemesModelBuild(), Color = ColorModelBuild(),
                            Title = TitleModelBuild(), Xlab = XlabModelBuild(),Ylab = YlabModelBuild(), LegendM = LegendMModelBuild(),LabelSize = LabelSizeModelBuild(),PAlpha = PAlphaModelBuild(),PSize = PSizeModelBuild())
          if (FontFamilyModelBuild() %in% 'Default') {
            p
          }else{
            FontFamilySet(P = p,Family = FontFamilyModelBuild())
          }
        }else if(GeomtypeModelBuild() %in% 'violin plus boxplot'){
          p = ViolinBox(df = dfModelBuild(),IVNumber = IVNumberModelBuild(),DepenVar = DepenVar2ModelBuild(),
                        Pred = PredictorModelBuild(),Modu1 = Modulator1ModelBuild(),Modu2 = Modulator2ModelBuild(),
                        Themes = ThemesModelBuild(), Color = ColorModelBuild(),
                        Title = TitleModelBuild(), Xlab = XlabModelBuild(),Ylab = YlabModelBuild(), LegendM = LegendMModelBuild(),LabelSize = LabelSizeModelBuild())
          if (FontFamilyModelBuild() %in% 'Default') {
            p
          }else{
            FontFamilySet(P = p,Family = FontFamilyModelBuild())
          }
        }else{
          if(IVNumberModelBuild() == 2){
            eval(parse(text = paste0('p = interactions::cat_plot(model = MModelBuild(), pred = ',PredictorModelBuild(),', ',
                                     'modx = ',Modulator1ModelBuild(),', ',
                                     'geom = ','\'',GeomtypeModelBuild(),'\'',', ',
                                     'errorbar.width = 0.2,',
                                     'legend.main = \'',LegendMModelBuild(),'\',',
                                     ifelse(GeomtypeModelBuild() %in% 'bar','','dodge.width = 0.3,'),
                                     'point.alpha = 0.1,',
                                     ifelse(ColorModelBuild() %in% c('Set1','Set2','Set3'),
                                            paste0('colors = \'', ColorModelBuild(),'\','),
                                            paste0('colors = c(',GreyBreaker(dfModelBuild(),Modulator1ModelBuild()),'),')),
                                     'plot.points = ',DotsModelBuild(),', geom.alpha = 0.8)',
                                     '+labs(y = YlabModelBuild(), x = XlabModelBuild(), title = TitleModelBuild())',
                                     ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSizeModelBuild()+5,'),',
                                     ' axis.title.x = element_text(size = ',LabelSizeModelBuild(),
                                     '), axis.title.y = element_text(size = ',LabelSizeModelBuild(),
                                     '), legend.text = element_text(size = ',LabelSizeModelBuild()-5,
                                     '), legend.title = element_text(size = ',LabelSizeModelBuild(),
                                     '), axis.text.y = element_text(size = ',LabelSizeModelBuild()-5,
                                     '), axis.text.x = element_text(size = ',LabelSizeModelBuild()-5,'))')))
            eval(parse(text = paste0('p = p',
                                     switch(ThemesModelBuild(),
                                            'origin' = '',
                                            'APA' = '+jtools::theme_apa()',
                                            'Solar' = '+ggthemes::theme_solarized()',
                                            'Wall Street Journal' = '+ggthemes::theme_wsj()',
                                            'Economist' = '+ggthemes::theme_economist()',
                                            'LibreOffice' = '+ggthemes::theme_calc()',
                                            'Google Docs' = '+ggthemes::theme_gdocs()',
                                            'Stata' = '+ggthemes::theme_stata()',
                                            'New Excel' = '+ggthemes::theme_excel_new()',
                                            'Ugly Excel(NEVER USE PLEASE)' = '+ggthemes::theme_excel()'))))
            if (FontFamilyModelBuild() %in% 'Default') {
              p
            }else{
              FontFamilySet(P = p,Family = FontFamilyModelBuild())
            }
          }else{
            eval(parse(text = paste0('p = interactions::cat_plot(model = MModelBuild(), pred = ',PredictorModelBuild(),', ',
                                     'modx = ',Modulator1ModelBuild(),', ',
                                     'mod2 = ',Modulator2ModelBuild(),', ',
                                     'geom = ','\'',GeomtypeModelBuild(),'\'',', ',
                                     'errorbar.width = 0.2,',
                                     'legend.main = \'',LegendMModelBuild(),'\',',
                                     ifelse(GeomtypeModelBuild() %in% 'bar','','dodge.width = 0.3,'),
                                     'point.alpha = 0.1,',
                                     ifelse(ColorModelBuild() %in% c('Set1','Set2','Set3'),
                                            paste0('colors = \'', ColorModelBuild(),'\','),
                                            paste0('colors = c(',GreyBreaker(dfModelBuild(),Modulator1ModelBuild()),'),')),
                                     'plot.points = ',DotsModelBuild(),', geom.alpha = 0.8)',
                                     '+labs(y = YlabModelBuild(), x = XlabModelBuild(), title = TitleModelBuild())',
                                     ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSizeModelBuild()+5,'),',
                                     ' axis.title.x = element_text(size = ',LabelSizeModelBuild(),
                                     '), axis.title.y = element_text(size = ',LabelSizeModelBuild(),
                                     '), legend.text = element_text(size = ',LabelSizeModelBuild()-5,
                                     '), legend.title = element_text(size = ',LabelSizeModelBuild(),
                                     '), axis.text.y = element_text(size = ',LabelSizeModelBuild()-5,
                                     '), axis.text.x = element_text(size = ',LabelSizeModelBuild()-5,'))')))
            eval(parse(text = paste0('p = p',
                                     switch(ThemesModelBuild(),
                                            'origin' = '',
                                            'APA' = '+jtools::theme_apa()',
                                            'Solar' = '+ggthemes::theme_solarized()',
                                            'Wall Street Journal' = '+ggthemes::theme_wsj()',
                                            'Economist' = '+ggthemes::theme_economist()',
                                            'LibreOffice' = '+ggthemes::theme_calc()',
                                            'Google Docs' = '+ggthemes::theme_gdocs()',
                                            'Stata' = '+ggthemes::theme_stata()',
                                            'New Excel' = '+ggthemes::theme_excel_new()',
                                            'Ugly Excel(NEVER USE PLEASE)' = '+ggthemes::theme_excel()'))))
            if (FontFamilyModelBuild() %in% 'Default') {
              p
            }else{
              FontFamilySet(P = p,Family = FontFamilyModelBuild())
            }
          }
        }


      }
    },width = function() return(WidthModelBuild()), height = function() return(HeightModelBuild()))

    output$Sub.PlotModelBuild = renderPlot({
      if(isTRUE(Split.SubModelBuild())){
        Density.Sub = function(df,Sub, DV, NumCol, transfer = 'Origin'){
          eval(parse(text = paste0('df$',Sub,' = factor(df$',Sub,')')))
          eval(parse(text = paste0('p = ggplot(data = df, aes(x = ',
                                   switch(transfer,
                                          'Origin' = DV,
                                          'Log E' = paste0('log(',DV,')'),
                                          'Log 10' = paste0('log10(',DV,')'),
                                          'Minus Reverse' = paste0('-1/',DV),
                                          'Minus Reverse Multi 1000' = paste0('-1000/',DV)),
                                   ', fill = ',Sub,'))+geom_density()')))
          eval(parse(text = paste0('p = p + facet_wrap(~',Sub,', ncol = ',NumCol,')')))
          eval(parse(text = paste('p + labs(x = \'',Sub,'\',', y = '\'',DV,'\')')))
        }
        p = Density.Sub(df = dfModelBuild(),Sub = SubNameModelBuild(),DV = DepenVarModelBuild(),NumCol = NumColModelBuild(),transfer = TransferModelBuild())
        print(p)
      }
    },width = function() return(WidthModelBuild()),
    height = function() return(HeightModelBuild()))

    output$InformationModelBuild = reactive({
      if(FormulaModelBuild() %in% c('Builder','Author','Inventor','Information')){
        'All the shiny interfaces are built by Zhangguangyao on his own.'
      }else{
        'This is the end.'
      }

    })

    FormulaPowercal = reactive(input$FormulaPowercal)
    RunOriginPowercal = reactive(input$RunOriginPowercal)
    FamilyPowercal = reactive(input$FamilyPowercal)
    FixedEffectPowercal = reactive(input$FixedEffectPowercal)
    SubjectPowercal = reactive(input$SubjectPowercal)
    MinSubPowercal = reactive(input$MinSubPowercal)
    MaxSubPowercal = reactive(input$MaxSubPowercal)
    StepPowercal = reactive(input$StepPowercal)
    NcorePowercal = reactive(input$NcorePowercal)
    NsimPowercal = reactive(input$NsimPowercal)
    obsPowercal = reactive(input$obsPowercal)

    dfPowercal = reactive({
      inFilePowercal <- input$file1Powercal

      if (is.null(inFilePowercal))
        return(NULL)

      rio::import(inFilePowercal$datapath)
    })
    output$DataSummaryPowercal = renderTable({
      head(dfPowercal(),n = obsPowercal())
    })

    TablePowercal = eventReactive(input$Run,{
      PowerTable(formula = FormulaPowercal(),family = FamilyPowercal(),
                 fixedeffect = FixedEffectPowercal(),subject = SubjectPowercal(),
                 minsub = MinSubPowercal(),maxsub = MaxSubPowercal(),steps = StepPowercal(),df = dfPowercal(),
                 Ncore = NcorePowercal(),Nsim = NsimPowercal(),RunOrigin = RunOriginPowercal())
    },ignoreNULL = F)

    output$PowerPowercal = renderTable({
      TablePowercal()
    })

    output$PlotPowercal = renderPlot({
      ggplot(data = TablePowercal(), aes(x = SubNumber, y = PowerValue))+
        geom_line()+geom_point(size=2)+
        geom_errorbar(aes(x = SubNumber, ymax = ConfUp, ymin = ConfLow), width = 0.1)+
        geom_hline(aes(yintercept = 80), linetype = 'dashed')+
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15))

    })

    output$downloadDataPowercal <- downloadHandler(
      filename = function() {
        paste('Power', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(TablePowercal(), file)
      }
    )
  }

  print(shinyApp(ui, server))
}

cat('\nThanks for using the Shiny user interface For Linear Mixed Model!\n\n')
cat('########################\nYou can run this command to lauch:\n
    LMM_Shinys()\n')
cat('\n########################\nPlease note that there will be continuous updates, so be sure to look out for it')
cat('\n\nPLEASE BE AWARE THAT bruceR package requires some later R version.
    So we suggest you use the R 3.6.1 as least.','\n\n')
ifelse(as.double(R.Version()$minor) >= 6.1,
       print('Your version is Fine'),
       print('Your version is a bit old. You might need to update R.'))
