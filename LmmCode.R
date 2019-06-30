
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(parallel)){install.packages('parallel')}
if(!require(shiny)){install.packages('shiny')}
if(!require(interactions)){install.packages('interactions')}
if(!require(lmerTest)){install.packages('lmerTest')}
if(!require(emmeans)){install.packages('emmeans')}
if(!require(jtools)){install.packages('jtools')}
if(!require(ggthemes)){install.packages('ggthemes')}
if(!require(simr)){install.packages('simr')}
if(!require(ggbeeswarm)){install.packages('ggbeeswarm')}
if(!require(rio)){install.packages('rio')}

####################
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
formula_generate_shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY formulas generator'),
    sidebarLayout(

      sidebarPanel(
        textInput('DV','Input the dependent variable:','Y'),

        selectInput('IVNumber','Select the number of fixed factors',choices = c(2,3)),

        textInput('IV1','Input the 1st factor:', 'A'),

        textInput('IV2','Input the 2nd factor:', 'B'),

        textInput('IV3','Input the 3rd factor:', 'C'),

        textInput('Cluster1','Input the 1st cluster variable:', 'Sub'),

        textInput('Cluster2','Input the 2nd cluster variable:', 'Item'),

        downloadButton("downloadData", "Download the formulas")
      )

      ,
      mainPanel(
        tableOutput("contents")
      )
    ))


  server <- function(input, output) {
    DV = reactive(input$DV)

    IVNumber = reactive(input$IVNumber)

    IV1 = reactive(input$IV1)

    IV2 = reactive(input$IV2)

    IV3 = reactive(input$IV3)

    Cluster1 = reactive(input$Cluster1)

    Cluster2 = reactive(input$Cluster2)


    Output = reactive(input$Output)

    output$contents <- renderTable({
      if (IVNumber() == 2) {
        formula_generate(DV = DV(),
                         IV = c(IV1(), IV2()),
                         Cluster = c(Cluster1(), Cluster2()))
      }else{
        formula_generate(DV = DV(),
                         IV = c(IV1(), IV2(),IV3()),
                         Cluster = c(Cluster1(), Cluster2()))
      }

    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$DV,'Formulas', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(formula_generate(DV = DV(),
                                   IV = c(IV1(), IV2()),
                                   Cluster = c(Cluster1(), Cluster2())) %>% as_tibble(), file)
      }
    )


  }

  print(shinyApp(ui, server))
}

####################
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
LMMRun_Parallel_shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY select the best fitted linear mixed model'),
    sidebarLayout(

      sidebarPanel(
        textInput('DV','Input the dependent variable:','Y'),

        selectInput('IVNumber','Select the number of fixed factors',choices = c(2,3)),

        textInput('IV1','Input the 1st factor:', 'A'),

        textInput('IV2','Input the 2nd factor:', 'B'),

        textInput('IV3','Input the 3rd factor:', 'C'),

        textInput('Cluster1','Input the 1st cluster variable:', 'Sub'),

        textInput('Cluster2','Input the 2nd cluster variable:', 'Item'),

        checkboxInput('Ifrun','Whether to run the models',T),

        checkboxInput('Manual','Whether to run models based on existing formulas',F),

        textInput('mfile', 'Input file name containing existing formulas:',NULL),

        selectInput('Family', 'Select the distribution family of dependent variable:',
                    choices = c('gaussian','binomial','poisson')),

        sliderInput('Ncore','Set the number of parallel cores', min = 1, max = 20, value = 4, step = 1),

        textInput('Output', 'input the prefix name of ouput file:','Y'),

        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),

        actionButton("update", "Update View")

      ),
      mainPanel(
        tableOutput("contents"),
        textOutput(outputId = 'End')
      )
    )
  )

  server <- function(input, output) {
    DV = reactive(input$DV)

    IVNumber = reactive(input$IVNumber)

    IV1 = reactive(input$IV1)

    IV2 = reactive(input$IV2)

    IV3 = reactive(input$IV3)

    Cluster1 = reactive(input$Cluster1)

    Cluster2 = reactive(input$Cluster2)

    Manual = reactive(input$Manual)

    Ifrun = reactive(input$Ifrun)

    FamilyD = reactive({
      switch(input$Family,
             "gaussian" = NULL,
             "binomial" = 'binomial',
             "poisson" = 'poisson')
    })


    Output = reactive(input$Output)

    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      mfile = reactive(input$mfile)

      if(is.null(mfile)){
        m = NULL
      }else{m = mfile()}

      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = rio::import(inFile$datapath)


      if(IVNumber() == 2){
        LMMRun_Parallel(df = d,
                        DV = DV(),
                        IV = c(IV1(), IV2()),
                        Cluster = c(Cluster1(), Cluster2()),
                        Manual = Manual(),Manualcodefilename = m, Family = FamilyD(),
                        Ifrun = Ifrun(),
                        Ncore = input$Ncore,
                        output = Output()) %>% arrange(Singular,-Converge,BIC)
      }else{
        LMMRun_Parallel(df = d,
                        DV = DV(),
                        IV = c(IV1(), IV2(),IV3()),
                        Cluster = c(Cluster1(), Cluster2()),
                        Manual = Manual(),Manualcodefilename = m, Family = FamilyD(),
                        Ifrun = Ifrun(),
                        Ncore = input$Ncore,
                        output = Output()) %>% arrange(Singular,-Converge,BIC)
      }
    })

    output$End = eventReactive(input$update,{
      if(Output() %in% c('Marry','Thank', 'Wey')){
        'Thank help from Wey. Would you marry me?'
      }else{'   '}
    },ignoreNULL = F)
  }

  print(shinyApp(ui, server))
}

####################
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
                         Title, Xlab, Ylab, LegendM, LabelSize){
  Height = eval(parse(text = paste0('(max(df$',DepenVar,')-min(df$',DepenVar,'))*0.007')))
  eval(parse(text = paste0('p = ggplot(data = df, aes(x = ',Pred,',y = ',DepenVar,', color = ',Modu1,'))+',
                           'geom_violin(alpha = 0,position = position_dodge(1))+',
                           'geom_quasirandom(dodge.width = 1, alpha = 0.2,bandwidth = 0.1)+',
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
LMM_Model_Info_Shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY linear mixed model builder'),
    sidebarLayout(

      sidebarPanel(
        helpText('Model Building Part:'),
        selectInput('HLM','Whether to perform the HLM or GLM?',choices = c('HLM','GLM')),
        textInput('Formula','Input the formula:',NULL),

        selectInput('Family', 'Select the distribution family of dependent variable:',
                    choices = c('gaussian','binomial','poisson')),

        selectInput('Contrasts','Select the type of contrasts:',
                    choices = c('sum','treatment')),

        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),
        numericInput("obs", "Set the number of observations to view:", 6),

        helpText('#######################'),
        helpText('Histogram on each Participants:'),
        checkboxInput('Split.Sub','Whether to plot histogram based on each subject?',F),
        selectInput('Transfer','Select type of data transfer',
                    choices = c('Origin', 'Log E', 'Log 10', 'Minus Reverse', 'Minus Reverse Multi 1000')),
        sliderInput('NumCol','How many columns should the histogram be arranged?',min = 1, max = 20,step = 1,value = 3),
        textInput('DepenVar','Input the name of column indication dependent variable',NULL),
        textInput('SubName','Input the name of column indicating subject',NULL),
        numericInput(inputId = 'Width',label = 'Set the plot Width',value = 400, min = 400, max = 10000,step = 1),
        numericInput(inputId = 'Height',label = 'Set the plot Height',value = 400, min = 400, max = 10000,step = 1),

        helpText('#######################'),
        helpText('Summary and Anova result download:'),
        downloadButton("downloadSummary", "Download the Summary table"),
        downloadButton("downloadAnova", "Download the Anova table"),

        helpText('#######################'),
        helpText('Simple effect analysis performer:'),
        checkboxInput('SimpleEffect',label = 'Whether to preform the simple effect analysis?',value = F),
        selectInput('IVNumber','Select the number of fixed factors',choices = c(2,3)),
        textInput('Predictor','Input the predictor`s name',NULL),
        textInput('Modulator1','Input the 1st modulator`s name',NULL),
        textInput('Modulator2','Input the 2nd modulator`s name if have',NULL),

        downloadButton("downloadEmmeans", "Download the Emmeans table"),
        downloadButton("downloadComparison", "Download the Comparison table"),

        helpText('#######################'),
        helpText('Set the parameters to plot:'),
        helpText('NOTE! Plot is based on the parameter you set in Simple effect analysis'),
        checkboxInput('Plot','Whether to plot',F),
        selectInput('Geomtype','Select the geometry to draw',
                    choices = c('bar','line','violin plus raw data','violin plus boxplot')),
        textInput('DepenVar2','Input the name of column indication dependent variable to plot violin',NULL),
        selectInput('Themes','Select the theme of plot:',
                    choices = c('origin','APA','Solar','Wall Street Journal',
                                'Economist','LibreOffice',
                                'Google Docs','Stata',
                                'New Excel','Ugly Excel(NEVER USE PLEASE)')),
        selectInput('Color','Select the color palette',
                    choices = c('Set1','Set2','Set3','Grey')),
        textInput('Title','Input the title of plot:',NULL),
        textInput('Ylab','Input the label of y axis:', NULL),
        textInput('Xlab','Input the label of x axis:', NULL),
        textInput('LegendM','Input the title of legend:', NULL),
        sliderInput(inputId = 'LabelSize',label = 'Set the size of plot labels and title',min = 10, max = 50,step = 1, value = 10),
        checkboxInput('Dots','Whether draw raw data (dots)?',F),
        numericInput(inputId = 'Width2',label = 'Set the plot Width',value = 400, min = 400, max = 10000,step = 1),
        numericInput(inputId = 'Height2',label = 'Set the plot Height',value = 400, min = 400, max = 10000,step = 1)

      ),
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel('Data Summary',tableOutput("DataSummary"))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Sub.Histogram',plotOutput('Sub.Plot',inline = T))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Model Summary',verbatimTextOutput("summary")),
                    tabPanel('Anova',tableOutput("Anova"))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Simple Effect',tableOutput('Emmeans'),tableOutput('Comparison'))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Plot', plotOutput('Plot',inline = T))),
        textOutput('Information')

      )
    )
  )

  server <- function(input, output) {

    HLM = reactive(input$HLM)
    Formula = reactive(input$Formula)

    Family = reactive(input$Family)

    obs = reactive(input$obs)

    Split.Sub = reactive(input$Split.Sub)
    Transfer = reactive(input$Transfer)
    NumCol = reactive(input$NumCol)
    SubName = reactive(input$SubName)
    DepenVar = reactive(input$DepenVar)
    Width = reactive(input$Width)
    Height = reactive(input$Height)

    SimpleEffect = reactive(input$SimpleEffect)
    IVNumber = reactive(input$IVNumber)
    Predictor = reactive(input$Predictor)
    Modulator1 = reactive(input$Modulator1)
    Modulator2 = reactive(input$Modulator2)

    PLOT = reactive(input$Plot)
    Geomtype = reactive(input$Geomtype)
    DepenVar2 = reactive(input$DepenVar2)
    Themes = reactive(input$Themes)
    Color = reactive(input$Color)
    Dots = reactive(input$Dots)
    Title = reactive(input$Title)
    Ylab = reactive(input$Ylab)
    Xlab = reactive(input$Xlab)
    LegendM = reactive(input$LegendM)
    LabelSize = reactive(input$LabelSize)
    Width2 = reactive(input$Width2)
    Height2 = reactive(input$Height2)

    Contrasts = reactive({
      switch(input$Contrasts,
             'sum' = 'contr.sum',
             'treatment' = 'contr.treatment')
    })

    df = reactive({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, header = T)
    })

    M = reactive({
      options(contrasts = c(Contrasts(),'contr.poly'))
      if(Family() %in% 'gaussian'){
        eval(parse(text = paste0(ifelse(HLM() %in% 'HLM', 'lmer','lm'),
                                 '(data = df(),formula = as.formula(Formula()))')))

      }else{
        eval(parse(text = paste0(ifelse(HLM() %in% 'HLM', 'glmer','glm'),
                                 '(data = df(),formula = as.formula(Formula()),family = ', Family(),')')))


      }
    })

    output$DataSummary = renderTable({

      head(df(),n = obs())
    })

    output$summary = renderPrint({
      summary(M())
    })
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste(input$DV,'Fixed_Effect_Table', ".csv", sep = "")
      },
      content = function(file) {

        M1 = round(summary(M())$coef,digits = 3)
        M1 = bind_cols(tibble(Effect = rownames(M1)),
                       as_tibble(M1))
        rio::export(M1, file)
      }
    )
    output$Anova = renderTable({
      M = anova(M())
      bind_cols(tibble(Effect = rownames(M)),
                as_tibble(M))


    })

    output$downloadAnova <- downloadHandler(

      filename = function() {
        paste(input$DV,'Anova_Table', ".csv", sep = "")
      },
      content = function(file) {
        M1 = round(anova(M()),digits = 3)
        M1 = bind_cols(tibble(Effect = rownames(M1)),
                       as_tibble(M1))
        rio::export(M1, file)
      }
    )

    output$Emmeans = renderTable({
      if(isTRUE(SimpleEffect())){

        if(IVNumber() == 2){
          eval(parse(text = paste0('emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),')$emm')))
        }else{
          eval(parse(text = paste0('emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$emm')))
        }
      }

    })

    output$downloadEmmeans = downloadHandler(

      filename = function() {
        paste(input$DV,'Emmeans_Table', ".csv", sep = "")
      },
      content = function(file) {
        if(isTRUE(SimpleEffect())){

          options(digits = 3)
          if(IVNumber() == 2){
            eval(parse(text = paste0('M1 = emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),')$emm %>% as_tibble()')))
          }else{
            eval(parse(text = paste0('M1 = emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$emm %>% as_tibble()')))
          }

        }
        rio::export(M1, file)
      }
    )

    output$Comparison = renderTable({
      if(isTRUE(SimpleEffect())){

        if(IVNumber() == 2){
          eval(parse(text = paste0('emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),')$contr %>% as_tibble()')))
        }else{
          eval(parse(text = paste0('emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$contr %>% as_tibble()')))
        }

      }
    })

    output$downloadComparison = downloadHandler(

      filename = function() {
        paste(input$DV,'Comparison_Table', ".csv", sep = "")
      },
      content = function(file) {
        if(isTRUE(SimpleEffect())){

          options(digits = 3)
          if(IVNumber() == 2){
            eval(parse(text = paste0('M1 = emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),')$contr %>% as_tibble()')))
          }else{
            eval(parse(text = paste0('M1 = emmeans(M(), pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$contr %>% as_tibble()')))
          }

        }
        rio::export(M1, file)
      }
    )

    output$Plot = renderPlot({
      if(isTRUE(PLOT())){

        if(Geomtype() %in% 'violin plus raw data'){

          ViolinRawdata(df = df(),IVNumber = IVNumber(),DepenVar = DepenVar2(),
                        Pred = Predictor(),Modu1 = Modulator1(),Modu2 = Modulator2(),
                        Themes = Themes(), Color = Color(),
                        Title = Title(), Xlab = Xlab(),Ylab = Ylab(), LegendM = LegendM(),LabelSize = LabelSize())
        }else if(Geomtype() %in% 'violin plus boxplot'){
          ViolinBox(df = df(),IVNumber = IVNumber(),DepenVar = DepenVar2(),
                    Pred = Predictor(),Modu1 = Modulator1(),Modu2 = Modulator2(),
                    Themes = Themes(), Color = Color(),
                    Title = Title(), Xlab = Xlab(),Ylab = Ylab(), LegendM = LegendM(),LabelSize = LabelSize())
        }else{
          if(IVNumber() == 2){
            eval(parse(text = paste0('p = interactions::cat_plot(model = M(), pred = ',Predictor(),', ',
                                     'modx = ',Modulator1(),', ',
                                     'geom = ','\'',Geomtype(),'\'',', ',
                                     'errorbar.width = 0.2,',
                                     'legend.main = \'',LegendM(),'\',',
                                     ifelse(Geomtype() %in% 'bar','','dodge.width = 0.3,'),
                                     'point.alpha = 0.1,',
                                     ifelse(Color() %in% c('Set1','Set2','Set3'),
                                            paste0('colors = \'', Color(),'\','),
                                            paste0('colors = c(',GreyBreaker(df(),Modulator1()),'),')),
                                     'plot.points = ',Dots(),', geom.alpha = 0.8)',
                                     '+labs(y = Ylab(), x = Xlab(), title = Title())',
                                     ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSize()+5,'),',
                                     ' axis.title.x = element_text(size = ',LabelSize(),
                                     '), axis.title.y = element_text(size = ',LabelSize(),
                                     '), legend.text = element_text(size = ',LabelSize()-5,
                                     '), legend.title = element_text(size = ',LabelSize(),
                                     '), axis.text.y = element_text(size = ',LabelSize()-5,
                                     '), axis.text.x = element_text(size = ',LabelSize()-5,'))')))
            eval(parse(text = paste0('p ',
                                     switch(Themes(),
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
          }else{
            eval(parse(text = paste0('p = interactions::cat_plot(model = M(), pred = ',Predictor(),', ',
                                     'modx = ',Modulator1(),', ',
                                     'mod2 = ',Modulator2(),', ',
                                     'geom = ','\'',Geomtype(),'\'',', ',
                                     'errorbar.width = 0.2,',
                                     'legend.main = \'',LegendM(),'\',',
                                     ifelse(Geomtype() %in% 'bar','','dodge.width = 0.3,'),
                                     'point.alpha = 0.1,',
                                     ifelse(Color() %in% c('Set1','Set2','Set3'),
                                            paste0('colors = \'', Color(),'\','),
                                            paste0('colors = c(',GreyBreaker(df(),Modulator1()),'),')),
                                     'plot.points = ',Dots(),', geom.alpha = 0.8)',
                                     '+labs(y = Ylab(), x = Xlab(), title = Title())',
                                     ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSize()+5,'),',
                                     ' axis.title.x = element_text(size = ',LabelSize(),
                                     '), axis.title.y = element_text(size = ',LabelSize(),
                                     '), legend.text = element_text(size = ',LabelSize()-5,
                                     '), legend.title = element_text(size = ',LabelSize(),
                                     '), axis.text.y = element_text(size = ',LabelSize()-5,
                                     '), axis.text.x = element_text(size = ',LabelSize()-5,'))')))
            eval(parse(text = paste0('p ',
                                     switch(Themes(),
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
        }


      }
    },width = function() return(Width2()), height = function() return(Height2()))

    output$Sub.Plot = renderPlot({
      if(isTRUE(Split.Sub())){
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
        p = Density.Sub(df = df(),Sub = SubName(),DV = DepenVar(),NumCol = NumCol(),transfer = Transfer())
        print(p)
      }
    },width = function() return(Width()),
    height = function() return(Height()))

    output$Information = reactive({
      if(Formula() %in% c('Builder','Author','Inventor','Information')){
        'All the shiny interfaces are built by Zhangguangyao on his own.'
      }else{
        'This is the end.'
      }

    })

  }

  print(shinyApp(ui, server))
}

####################
Datafilter = function(NGroup,df, DV, FilterO = T,
                      Group1=NULL, Group2=NULL, Group3=NULL, Group4 = NULL, Group5 = NULL, ZV = 3){
  if(NGroup == 0){
    eval(parse(text = paste0('df2 = df ','%>% ',
                             ifelse(isTRUE(FilterO), paste0('filter(',DV,' != 0) %>% '),''),
                             ' mutate(Zvalue = scale(',DV,'))',' %>% ',
                             'filter(abs(Zvalue) < ',ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 1){
    eval(parse(text = paste0('df2 = df %>% ',
                             ifelse(isTRUE(FilterO), paste0('filter(',DV,' != 0) %>% '),''),
                             'group_by(',Group1,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 2){
    eval(parse(text = paste0('df2 = df %>% ',
                             ifelse(isTRUE(FilterO), paste0('filter(',DV,' != 0) %>% '),''),
                             'group_by(',Group1,', ',Group2,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 3){
    eval(parse(text = paste0('df2 = df %>% ',
                             ifelse(isTRUE(FilterO), paste0('filter(',DV,' != 0) %>% '),''),
                             'group_by(',Group1,', ',Group2,', ',Group3,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 4){
    eval(parse(text = paste0('df2 = df %>% ',
                             ifelse(isTRUE(FilterO), paste0('filter(',DV,' != 0) %>% '),''),
                             'group_by(',Group1,', ',Group2,', ',Group3,', ',Group4,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 5){
    eval(parse(text = paste0('df2 = df %>% ',
                             ifelse(isTRUE(FilterO), paste0('filter(',DV,' != 0) %>% '),''),
                             'group_by(',Group1,', ',Group2,', ',Group3,', ',Group4,', ',Group5,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  return(df2)
}
Data_Filter_Shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY Data filter'),
    sidebarLayout(

      sidebarPanel(
        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),

        textInput('DV','Input the dependent variable:',NULL),

        checkboxInput('Filter0','Whether to filter the data equal to 0',value = F),

        selectInput('NumGroup','Select the number of categories',choices = c(0:5)),

        textInput('G1','Input the 1st factor:', NULL),

        textInput('G2','Input the 2nd factor:', NULL),

        textInput('G3','Input the 3rd factor:', NULL),

        textInput('G4','Input the 4th factor:', NULL),

        textInput('G5','Input the 5th factor:', NULL),

        sliderInput('ZV','Set the Z value to filter',min = 1, max = 5,step = 0.1, value = 3),

        numericInput("obs", "Set the number of observations to view:", 6),

        downloadButton("downloadData", "Download the filtered Data")
      )

      ,
      mainPanel(
        h4('Data raw Summary'),
        tableOutput("DataSummary"),
        textOutput('OldLine'),
        h4('Data filtered Summary:'),
        tableOutput('DataFiltered'),
        textOutput('NewLine')
      )
    ))


  server <- function(input, output) {
    DV = reactive(input$DV)
    Filter0 = reactive(input$Filter0)
    NumGroup = reactive(input$NumGroup)
    G1 = reactive(input$G1)
    G2 = reactive(input$G2)
    G3 = reactive(input$G3)
    G4 = reactive(input$G4)
    G5 = reactive(input$G5)
    ZV = reactive(input$ZV)
    obs = reactive(input$obs)


    output$DataSummary = renderTable({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = rio::import(inFile$datapath)
      head(d,n = obs())
    })

    output$OldLine = renderText({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = rio::import(inFile$datapath)
      print(paste0('There are ', nrow(d),' lines.'))
    })

    output$DataFiltered = renderTable({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = rio::import(inFile$datapath)
      df = Datafilter(NGroup = NumGroup(), df = d,FilterO = Filter0(),
                      DV = DV(),Group1 = G1(),Group2 = G2(),Group3 = G3(),Group4 = G4(),Group5 = G5(),
                      ZV = ZV())
      head(df, n = obs())
    })

    output$NewLine = renderText({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = rio::import(inFile$datapath)
      df = Datafilter(NGroup = NumGroup(), df = d,FilterO = Filter0(),
                      DV = DV(),Group1 = G1(),Group2 = G2(),Group3 = G3(),Group4 = G4(),Group5 = G5(),
                      ZV = ZV())
      print(paste0('There are ', nrow(df),' lines.'))
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$DV,'Filtered', ".csv", sep = "")
      },
      content = function(file) {
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = rio::import(inFile$datapath)
        df = Datafilter(NGroup = NumGroup(), df = d,FilterO = Filter0(),
                        DV = DV(),Group1 = G1(),Group2 = G2(),Group3 = G3(),Group4 = G4(),Group5 = G5(),
                        ZV = ZV())
        rio::export(df, file)
      }
    )


  }

  print(shinyApp(ui, server))
}

####################
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
Power_Shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY Power calculation of linear mixed model'),
    sidebarLayout(

      sidebarPanel(
        actionButton(inputId = 'Run',label = 'Run Power Calculation !!'),
        actionButton(inputId = 'Clear',label = 'Clear all variables in working space.'),

        textInput('Formula','Input the formula:',NULL),

        checkboxInput(inputId = 'RunOrigin',label = 'Whether run the origin model?',value = T),

        selectInput('Family', 'Select the distribution family of dependent variable:',
                    choices = c('gaussian','binomial','poisson')),

        textInput('FixedEffect','Input the fixed effect to examine:',NULL),

        textInput('Subject','Input the name of variable indicating participant/item:',NULL),

        numericInput('MinSub','Input the minimum number of participants/item to check:',20),
        numericInput('MaxSub','Input the maximum number of participants/item to check:',80),

        sliderInput('Step','Set the step to increase:',min = 1, max = 20,step = 1,value = 10),

        numericInput('Nsim','Input the number of simulations:',100),

        sliderInput('Ncore','Set the paralle cores to run',min = 1, max = 20, step = 1, value = 4),

        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),
        numericInput("obs", "Set the number of observations to view:", 6),
        downloadButton("downloadData", "Download the power table")
      ),
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel('Data Summary',tableOutput("DataSummary"))),

        tabsetPanel(type = 'tabs',
                    tabPanel('Power table',tableOutput("Power"))),

        tabsetPanel(type = 'tabs',
                    tabPanel('Plot', plotOutput('Plot')))

      )
    )
  )

  server <- function(input, output) {

    Formula = reactive(input$Formula)
    RunOrigin = reactive(input$RunOrigin)
    Family = reactive(input$Family)
    FixedEffect = reactive(input$FixedEffect)
    Subject = reactive(input$Subject)
    MinSub = reactive(input$MinSub)
    MaxSub = reactive(input$MaxSub)
    Step = reactive(input$Step)
    Ncore = reactive(input$Ncore)
    Nsim = reactive(input$Nsim)
    obs = reactive(input$obs)

    df = reactive({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, header = T)
    })
    output$DataSummary = renderTable({
      head(df(),n = obs())
    })

    Table = eventReactive(input$Run,{
      PowerTable(formula = Formula(),family = Family(),
                 fixedeffect = FixedEffect(),subject = Subject(),
                 minsub = MinSub(),maxsub = MaxSub(),steps = Step(),df = df(),
                 Ncore = Ncore(),Nsim = Nsim(),RunOrigin = RunOrigin())
    },ignoreNULL = F)

    output$Power = renderTable({
      Table()
    })

    output$Plot = renderPlot({
      ggplot(data = Table(), aes(x = SubNumber, y = PowerValue))+
        geom_line()+geom_point(size=2)+
        geom_errorbar(aes(x = SubNumber, ymax = ConfUp, ymin = ConfLow), width = 0.1)+
        geom_hline(aes(yintercept = 80), linetype = 'dashed')+
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15))

    })

    eventReactive(input$Clear,{
      rm(list = ls())
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste('Power', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(Table(), file)
      }
    )
  }

  print(shinyApp(ui, server))
}

####################
cat('\nThanks for using the Shiny user interface For Linear Mixed Model!\n\n')
cat('Now there are several functions in your environment.\n
    You can simply run some of them to satisfy some of your need.\n\n')
cat('########################\n1.You can run this command to generate and download all formulas you might need:\n
    formula_generate_shiny()\n\n')
cat('########################\n2.You can run this command to run all possible models in order to select the best fitted one:\n
    LMMRun_Parallel_shiny()\n\n')
cat('########################\n3.You can run this command to build a model and get the summary, anova information.\nYou can also perform simple effect analysis and generate good-looking plot:\n
    LMM_Model_Info_Shiny()\n\n')
cat('########################\n4.You can run this command to filter data:\n
    Data_Filter_Shiny()\n\n########################\n\n')
cat('########################\n5.You can run this command to calculate power and predict the size of subjects:\n
    Power_Shiny()\n\n########################\n\n')
cat('For more details and usages, please refer to the links below:\n
    https://zhuanlan.zhihu.com/p/67680257\n
    https://zhuanlan.zhihu.com/p/67048151\n
    https://zhuanlan.zhihu.com/p/63092231\n
    https://zhuanlan.zhihu.com/p/68469202\n')
cat('\n########################\nPlease note that there will be continuous updates, so be sure to look out for it')
