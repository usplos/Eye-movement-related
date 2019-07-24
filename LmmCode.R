
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

    Formulas = reactive({
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


    output$contents <- renderTable({
      Formulas() %>% as_tibble()
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$DV,'Formulas', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(Formulas() %>% as_tibble(), file)
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
LMMRun_Parallel_shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY select the best fitted linear mixed model'),
    sidebarLayout(

      sidebarPanel(
        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),
        numericInput("obs", "Set the number of observations to view:", 6),

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

        actionButton("Run", "Run!")

      ),
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel('Raw data',tableOutput("Raw")),
                    tabPanel('Model information',tableOutput("contents"))),
        textOutput(outputId = 'End')
      )
    )
  )

  server <- function(input, output) {
    obs = reactive(input$obs)
    DV = reactive(input$DV)

    IVNumber = reactive(input$IVNumber)

    IV1 = reactive(input$IV1)

    IV2 = reactive(input$IV2)

    IV3 = reactive(input$IV3)

    Cluster1 = reactive(input$Cluster1)

    Cluster2 = reactive(input$Cluster2)

    Manual = reactive(input$Manual)
    mfile = reactive(input$mfile)

    Ifrun = reactive(input$Ifrun)

    FamilyD = reactive({
      switch(input$Family,
             "gaussian" = NULL,
             "binomial" = 'binomial',
             "poisson" = 'poisson')
    })


    Output = reactive(input$Output)

    df = reactive({
      mfile = reactive(input$mfile)

      if(is.null(mfile)){
        m = NULL
      }else{m = mfile()}

      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      rio::import(inFile$datapath)
    })

    ModelInfo = eventReactive(input$Run,{
      if(IVNumber() == 2){
        LMMRun_Parallel(df = df(),
                        DV = DV(),
                        IV = c(IV1(), IV2()),
                        Cluster = c(Cluster1(), Cluster2()),
                        Manual = Manual(),Manualcodefilename = mfile(), Family = FamilyD(),
                        Ifrun = Ifrun(),
                        Ncore = input$Ncore,
                        output = Output())  %>% arrange(Singular,-Converge,-Nchar) %>%
          select(-Nchar)
      }else{
        LMMRun_Parallel(df = df(),
                        DV = DV(),
                        IV = c(IV1(), IV2(),IV3()),
                        Cluster = c(Cluster1(), Cluster2()),
                        Manual = Manual(),Manualcodefilename = mfile(), Family = FamilyD(),
                        Ifrun = Ifrun(),
                        Ncore = input$Ncore,
                        output = Output()) %>% arrange(Singular,-Converge,-Nchar) %>%
          select(-Nchar)
      }
    },ignoreNULL = T)

    output$Raw = renderTable({
      head(df(),obs())
    })

    output$contents <- renderTable({
      ModelInfo()
    })

    output$End = eventReactive(input$Run,{
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
LMM_Model_Info_Shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY linear mixed model builder'),
    sidebarLayout(

      sidebarPanel(
        helpText('Model Building Part:'),
        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),
        numericInput("obs", "Set the number of observations to view:", 6),

        selectInput('HLM','Whether to perform the HLM or GLM?',choices = c('HLM','GLM')),
        textInput('Formula','Input the formula:',NULL),

        selectInput('Family', 'Select the distribution family of dependent variable:',
                    choices = c('gaussian','binomial','poisson')),

        selectInput('Contrasts','Select the type of contrasts:',
                    choices = c('sum','treatment')),

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
        numericInput('PAlpha','Set the point alpha for Violin plot',value = 0.2, min = 0,max = 1),
        numericInput('PSize','Set the point Size for Violin plot',value = 1,step = 0.1),
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
        selectInput(inputId = 'FontFamily',label = 'Set the family of font in plot',
                    choices = c('Default','Times New Roman','SimSun','Cambria','Times','Arial','Calibri'),selected = 'Default'),
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
    PAlpha = reactive(input$PAlpha)
    PSize = reactive(input$PSize)
    Themes = reactive(input$Themes)
    Color = reactive(input$Color)
    Dots = reactive(input$Dots)
    Title = reactive(input$Title)
    Ylab = reactive(input$Ylab)
    Xlab = reactive(input$Xlab)
    LegendM = reactive(input$LegendM)
    LabelSize = reactive(input$LabelSize)
    FontFamily = reactive(input$FontFamily)
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

        FontFamilySet = function(P,Family = 'Times'){
          p + theme(title = element_text(family = Family),
                    text = element_text(family = Family))
        }

        if(Geomtype() %in% 'violin plus raw data'){

          p = ViolinRawdata(df = df(),IVNumber = IVNumber(),DepenVar = DepenVar2(),
                        Pred = Predictor(),Modu1 = Modulator1(),Modu2 = Modulator2(),
                        Themes = Themes(), Color = Color(),
                        Title = Title(), Xlab = Xlab(),Ylab = Ylab(), LegendM = LegendM(),LabelSize = LabelSize(),PAlpha = PAlpha(),PSize = PSize())
          if (FontFamily() %in% 'Default') {
            p
          }else{
            FontFamilySet(P = p,Family = FontFamily())
          }
        }else if(Geomtype() %in% 'violin plus boxplot'){
          p = ViolinBox(df = df(),IVNumber = IVNumber(),DepenVar = DepenVar2(),
                    Pred = Predictor(),Modu1 = Modulator1(),Modu2 = Modulator2(),
                    Themes = Themes(), Color = Color(),
                    Title = Title(), Xlab = Xlab(),Ylab = Ylab(), LegendM = LegendM(),LabelSize = LabelSize())
          if (FontFamily() %in% 'Default') {
            p
          }else{
            FontFamilySet(P = p,Family = FontFamily())
          }
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
            eval(parse(text = paste0('p = p',
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
            if (FontFamily() %in% 'Default') {
              p
            }else{
              FontFamilySet(P = p,Family = FontFamily())
            }
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
            eval(parse(text = paste0('p = p',
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
            if (FontFamily() %in% 'Default') {
              p
            }else{
              FontFamilySet(P = p,Family = FontFamily())
            }
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
        
        numericInput("obs", "Set the number of observations to view:", 6),

        textInput('DV','Input the dependent variable:',NULL),

        checkboxInput('Filter0','Whether to filter the data equal to 0',value = F),
        
        checkboxInput('FilterNA','Whether to filter the NA data',value = T),

        textInput('Group','Input the group to categorise (seperated by comma). If there is no subgroup, need not input.',NULL),

        sliderInput('ZV','Set the Z value to filter',min = 1, max = 5,step = 0.1, value = 3),

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
    FilterNA = reactive(input$FilterNA)
    Group = reactive(input$Group)
    ZV = reactive(input$ZV)
    obs = reactive(input$obs)
    df = reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      rio::import(inFile$datapath)
    })
    df2 = reactive({
      Datafilter(df = df(),
                 DV = DV(),
                 FilterO = Filter0(),
                 FilterNA = FilterNA(),
                 Group = Group(),
                 ZV = ZV())
    })

    output$DataSummary = renderTable({
      head(df(),n = obs())
    })

    output$OldLine = renderText({
      print(paste0('There are ', nrow(df()),' lines.'))
    })

    output$DataFiltered = renderTable({
      head(df2(), n = obs())
    })

    output$NewLine = renderText({
      print(paste0('There are ', nrow(df2()),' lines.'))
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$DV,'Filtered', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(df2(), file)
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

        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),
        numericInput("obs", "Set the number of observations to view:", 6),
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

SV = function(Data,Sub, IV, DV, bootstrapNumber = 1000, perbinMax = 300, perbinMin = 0, baseline, NumSig = 5,
              Ylab = 'DV', Xlab = 'IV', LegendM, WordSize, Title)
{
  ############ calculate
  DataRaw = Data
  CondUnique = unique(DataRaw[[IV]])

  for(i in CondUnique){
    eval(parse(text = paste('perbin',i,' = matrix(0,nrow = bootstrapNumber, ncol = perbinMax-perbinMin+1)', sep = '')))
  }

  for(condition in CondUnique){
    CondTemp = matrix(0, nrow = bootstrapNumber, ncol = perbinMax - perbinMin+1)
    eval(parse(text = paste0('DataRawSS = filter(DataRaw ,',IV,' %in% condition)')))
    eval(parse(text = paste0('SubjectUnique = unique(DataRawSS$',Sub,')')))
    for(subject in SubjectUnique)
    {
      eval(parse(text = paste0('DataRawS = DataRawSS %>% filter(',Sub,' %in% subject)')))
      SubCondTemp = matrix(0, nrow = bootstrapNumber, ncol = perbinMax - perbinMin+1)
      for (i in 1:bootstrapNumber)
      {
        eval(parse(text = paste0('SubCondTempOnce = sort(sample(DataRawS$',DV,', size = nrow(DataRawS), replace = T))')))
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
    eval(parse(text = paste('perbin',condition,' = CondTemp/length(SubjectUnique)', sep = '')))
    remove(CondTemp)
    cat(condition, ' is done\n###############################################################\n')
  }

  CondCompare = eval(parse(text = paste('perbin',CondUnique[-1 * which(CondUnique %in% baseline)],' - ', 'perbin',baseline, sep = '')))
  Differ = numeric(perbinMax - perbinMin +1)
  for(i in 1:(perbinMax - perbinMin +1)){
    Differ[i] = mean(CondCompare[,i]>0)
  }
  check = 0;TimePoint = NA;
  for (i in 1:(perbinMax - perbinMin + 1 - NumSig + 1)) {
    if(mean(Differ[i:(i+NumSig - 1)] > 0.95) == 1){
      TimePoint = i;
      check = 1;break
    }
  }
  Result = list()
  Result[[1]] = check
  if(check == 1){
    #Result[[1]] = paste0('The first time point is ',TimePoint,' ms.')
    eval(parse(text = paste('perbinA',CondUnique[-1 * which(CondUnique %in% baseline)],' = numeric()', sep = '')))
    eval(parse(text = paste('perbinA',baseline,' = numeric()', sep = '')))
    for(i in 1:(perbinMax - perbinMin + 1)){
      eval(parse(text = paste('perbinA',CondUnique[-1 * which(CondUnique %in% baseline)],'[i] = mean(','perbin',CondUnique[-1 * which(CondUnique %in% baseline)],'[,i])', sep = '')))
      eval(parse(text = paste('perbinA',baseline,'[i] = mean(','perbin',baseline,'[,i])', sep = '')))
    }

    eval(parse(text = paste('file = tibble(Cond',baseline,' = ','perbinA',baseline,', Cond',CondUnique[-1 * which(CondUnique %in% baseline)],' = ', 'perbinA',CondUnique[-1 * which(CondUnique %in% baseline)],', Time = seq(perbinMin,perbinMax,1))',sep = '')))

    Result[[2]] = file
    Result[[3]] = TimePoint+perbinMin
    Colname = names(file)
    DF = file %>% gather(Condition,Value,-Time)
    Result[[4]]= ggplot(data = DF, aes(x = Time, y = Value, color = Condition))+
      geom_line(aes(group = Condition),size=1.2)+
      scale_color_manual(values = c('grey10','grey50'))+
      geom_vline(xintercept = TimePoint+perbinMin, linetype=3, size=0.8)+
      labs(x = Xlab, y = Ylab,color = LegendM, title = Title)+
      theme(axis.title = element_text(size = WordSize),
            axis.text = element_text(size = WordSize-3),
            legend.title = element_text(size = WordSize),
            legend.text = element_text(size = WordSize-3))
  }

  return(Result)
}


SurvivalAnalysis_Shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY Survival Analysis'),
    sidebarLayout(

      sidebarPanel(
        actionButton(inputId = 'Run',label = 'Run!'),
        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),
        numericInput(inputId = 'obs','Set the number of row to scan',value = 6),

        textInput('Sub','Input the name the variable of participant:',NULL),
        textInput('IV','Input the name the variable of independent variable:',NULL),
        textInput('DV','Input the name the variable of dependent variable:',NULL),
        numericInput('bootstrapNumber','Input the number of bootstrap:',value = 1000),
        numericInput('NumSig', 'Set the number of contineous significance:',value = 5,step = 1),
        numericInput('perbinMax','Input the maximum time bin:',value = 600),
        numericInput('perbinMin','Input the minimum time bin:',value = 0),
        textInput('baseline','Input the name of baseline condition',NULL),
        helpText('#######'),
        helpText('Plot parameters'),
        textInput('Title','Input the main title',NULL),
        textInput('Xlab','Input the title of x axis',NULL),
        textInput('Ylab','Input the title of y axis',NULL),
        textInput('LegendM','Input the title of legend',NULL),
        numericInput(inputId = 'WordSize',label = 'Set the size of words',step = 0.1,value = 15),

        downloadButton("downloadTable", "Download the Survival Table")
      )

      ,
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel('Raw Data Summary',tableOutput("DataSummary"))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Divergent time',textOutput("TimePoint")),
                    tabPanel('Survival Table', tableOutput('SurvivalTable'))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Plot', plotOutput('Plot')))

      )
    ))
  server <- function(input, output) {
    obs = reactive(input$obs)
    Sub = reactive(input$Sub)
    IV = reactive(input$IV)
    DV = reactive(input$DV)
    bootstrapNumber = reactive(input$bootstrapNumber)
    NumSig = reactive(input$NumSig)
    perbinMax = reactive(input$perbinMax)
    perbinMin = reactive(input$perbinMin)
    baseline = reactive(input$baseline)
    Title = reactive(input$Title)
    Xlab = reactive(input$Xlab)
    Ylab = reactive(input$Ylab)
    LegendM = reactive(input$LegendM)
    WordSize = reactive(input$WordSize)

    df = reactive({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      rio::import(inFile$datapath)
    })

    output$DataSummary <- renderTable({
      head(df(),obs())
    })

    Table = eventReactive(input$Run,{
      SV(Data = df(),Sub = Sub(),IV = IV(),DV = DV(),bootstrapNumber = bootstrapNumber(),
         perbinMax = perbinMax(),perbinMin = perbinMin(),baseline = baseline(),Ylab = Ylab(),
         Xlab = Xlab(),LegendM = LegendM(),WordSize = WordSize(),Title = Title(),NumSig = NumSig())
    })

    output$TimePoint = renderText({
      if(Table()[[1]] == 1){
      paste('The divergent time point is ', Table()[[3]],' ms.')
      }else{
        paste0('There is no divergent time point.')
      }
    })

    output$SurvivalTable = renderTable({
      if(Table()[[1]] == 1){
        head(Table()[[2]],obs())
      }
    })

    output$Plot = renderPlot({
      if(Table()[[1]] == 1){
        Table()[[4]]+
          labs(x = Xlab(), y = Ylab(),color = LegendM(), title = Title())+
          theme(plot.title = element_text(hjust = 0.5, size = WordSize()+3),
                axis.title = element_text(size = WordSize()),
                axis.text = element_text(size = WordSize()-3),
                legend.title = element_text(size = WordSize()),
                legend.text = element_text(size = WordSize()-3))
      }
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste('SurvivalTable', ".csv", sep = "")
      },
      content = function(file) {
        rio::export(Table()[[2]], file)
      }
    )

  }

  print(shinyApp(ui, server))
}

TimeBinGenrate = function(df,Categories = 'Sub,Number,AOI', OnsetName, rangemin, rangemax, steps = 100){
  Category = strsplit(Categories,',') %>% unlist()
  df = eval(parse(text = paste0('df %>% mutate(Cond = paste(',Categories,', sep=\'_\'))')))

  Function = function(d){
    eval(parse(text = paste0('d %>% filter(', OnsetName,' < rangemax, ', OnsetName,' > rangemin) %>% mutate(TimeBins = (',OnsetName,
                             '-rangemin) %/% steps + 1)')))
  }

  dfNew = df %>% split(.$Cond) %>% map(function(d) Function(d))
  dfNew2 = tibble()
  for(dd in 1:length(dfNew)){
    dfNew2 = rbind(dfNew2, dfNew[[dd]])
  }
  dfNew2 = dfNew2 %>% select(-Cond)
  eval(parse(text = paste0('dfNew2 %>% group_by(',Category[1:(length(Category)-1)] %>% paste0(collapse = ','),
                           ', TimeBins) %>% mutate(FixNum = length(TimeBins)) %>% group_by(',Categories,
                           ', TimeBins) %>% summarise(FixProp = length(TimeBins)/unique(FixNum))')))

}

GrowthCurvePlot = function(df,Categories = 'Sub,AOI', OnsetName, rangemin, rangemax, steps = 100,
                           Xlab, Ylab, legendMain,Psize,Textsize,FontFamily='Default', ColorVar = NULL, FacetVar){
  Category = strsplit(Categories,',') %>% unlist()
  df = eval(parse(text = paste0('df %>% mutate(Cond = paste(',Categories,', sep=\'_\'))')))

  Command1 = ''
  for (cc in Category) {
    Command1 = paste0(Command1,' %>% split(.$',cc,') %>% map(function(d){d ',collapse = '')
  }
  Command1 = paste0('dfNew = df', substr(Command1,start = 1,stop = nchar(Command1)-2))

  Function = function(d){
    eval(parse(text = paste0('d %>% filter(', OnsetName,' < rangemax, ', OnsetName,' > rangemin) %>% mutate(TimeBins = (',OnsetName,
                             '-rangemin) %/% steps + 1)')))
  }

  dfNew = df %>% split(.$Cond) %>% map(function(d) Function(d))
  dfNew2 = tibble()
  for(dd in 1:length(dfNew)){
    dfNew2 = rbind(dfNew2, dfNew[[dd]])
  }

  eval(parse(text = paste0('dfNew3 = dfNew2 %>% group_by(TimeBins) %>% mutate(FixNum = length(TimeBins)) %>% group_by(',Category[-1] %>% paste0(collapse = ','),
                           ', TimeBins) %>% summarise(FixProp = length(TimeBins)/unique(FixNum)) %>% mutate(Time = rangemin+steps*TimeBins+steps/2)')))

  Xlab = paste0('\n',Xlab)
  Ylab = paste0(Ylab,'\n')
  if(is.null(ColorVar)){
    ColorVar = Category[[length(Category)]]
    FacetVar = Category[[2]]
  }
  eval(parse(text = paste0('p = ggplot(data = dfNew3, aes(x = Time, y = FixProp, color = ',ColorVar,' %>% factor()))+',
                           'geom_point(size = Psize)+ geom_line(aes(group = ',ColorVar,'))+',
                           'labs(x = Xlab, y = Ylab, color = legendMain)+',
                           'scale_color_brewer(palette = \'Set1\')',
                           ifelse(length(Category)>2,
                                  paste0('+facet_wrap(~',FacetVar,', ncol=1)+',
                                         'theme(strip.text = element_text(size = ',Textsize,'))'),
                                  ''),
                           '+theme(axis.text = element_text(size = ',Textsize-3,'),',
                           'axis.title = element_text(size = ',Textsize,'),',
                           'legend.text = element_text(size = ',Textsize-3,'),',
                           'legend.title = element_text(size = ',Textsize,'))')))
  FontFamilySet = function(P,Family = 'Times'){
    p + theme(title = element_text(family = Family),
              text = element_text(family = Family))
  }

  if (FontFamily %in% 'Default') {
    p
  }else{
    FontFamilySet(P = p,Family = FontFamily)
  }
}

GrowthCurveAnalysis_shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY Growth Curve Analysis'),
    sidebarLayout(

      sidebarPanel(
        fileInput("file1", "Choose the File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",'.xls','.txt','.xlsx')
        ),

        textInput('Categories','Input the Categories (seperated by comma, the first one should be the name of subject, the last one should be the name of AOI):', NULL),

        textInput('OnsetName','Input the name of the variable of the onsettime:', NULL),

        numericInput('rangemin','Input the minimum onsettime:', value = 0),

        numericInput('rangemax','Input the maximum onsettime:', value = 1000),

        numericInput('steps','Input the steps to increase:', value = 100),

        numericInput("obs", "Set the number of observations to view:", 6),

        textInput('Formula','Input the formula:',NULL),
        numericInput('NumPower', 'Set the highest power:',value = 4,step = 1),

        checkboxInput('Plot',label = 'Whether to plot growth curve',value = F),
        textInput('Xlab', 'Input the x axis label:',NULL),
        textInput('Ylab', 'Input the y axis label:',NULL),
        textInput('Llab', 'Input the legend main:',NULL),
        textInput('ColorVar','Set the Color of point based on the name of variable',NULL),
        textInput('FacetVar','Set the Facet of the plot based on the name of variable',NULL),
        numericInput(inputId = 'Psize',label = 'Set the size of point',value = 0.5,step = 0.01),
        numericInput(inputId = 'Textsize',label = 'Set the size of text',value = 15,step = 0.1),
        selectInput(inputId = 'FontFamily',label = 'Set the family of font in plot',
                    choices = c('Default','Times New Roman','SimSun','Cambria','Times','Arial','Calibri'),selected = 'Default'),
        numericInput(inputId = 'Width',label = 'Set the plot Width',value = 400,step = 1),
        numericInput(inputId = 'Height',label = 'Set the plot Height',value = 400,step = 1),

        downloadButton("downloadSummary", "Download the Summary"),
        downloadButton("downloadANOVA", "Download the ANOVA"),
        downloadButton("downloadfixpropdata", "Download the data for plot"),
        downloadButton("downloadanalysisdata", "Download the data for analysis")
      )

      ,
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel('Raw Data Summary',tableOutput("DataSummary")),
                    tabPanel('Data Summary with fix proportion',tableOutput("DataSummary2")),
                    tabPanel('Data Summary to analysis',tableOutput("DataSummary3"))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Model Summary',tableOutput("summary")),
                    tabPanel('Anova',tableOutput("Anova"))),
        tabsetPanel(type = 'tabs',
                    tabPanel('Plot', plotOutput('Plot',inline = T)))

      )
    ))
  server <- function(input, output) {
    Categories = reactive(input$Categories)
    OnsetName = reactive(input$OnsetName)
    rangemin = reactive(input$rangemin)
    rangemax = reactive(input$rangemax)
    steps = reactive(input$steps)
    obs = reactive(input$obs)
    Formula = reactive(input$Formula)
    NumPower = reactive(input$NumPower)
    Plot = reactive(input$Plot)
    Xlab = reactive(input$Xlab)
    Ylab = reactive(input$Ylab)
    Llab = reactive(input$Llab)
    ColorVar = reactive(input$ColorVar)
    FacetVar = reactive(input$FacetVar)
    Psize = reactive(input$Psize)
    Textsize = reactive(input$Textsize)
    FontFamily = reactive(input$FontFamily)
    Width = reactive(input$Width)
    Height = reactive(input$Height)

    df = reactive({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      rio::import(inFile$datapath)
    })

    output$contents0 = renderTable({
      head(df(),6)
    })

    Table = reactive({
      TimeBinGenrate(df = df(),
                     Categories = Categories(),
                     OnsetName = OnsetName(),
                     rangemin = rangemin(),
                     rangemax = rangemax(),
                     steps = steps())
    })

    df3 = reactive({
      Category = strsplit(Categories(),',') %>% unlist()
      dfAnalysis = function(df){
        eval(parse(text = paste0('df$',Category[[2]],' = factor(df$',Category[[2]],')')))
        Time = poly(unique(df$TimeBins),NumPower())
        df[,paste0('ot',1:NumPower())] = Time[df$TimeBins - min(df$TimeBins)+1,1:NumPower()]
        return(df)
      }
      dfAnalysis(Table())
    })


    output$DataSummary <- renderTable({
      head(df(),obs())
    })
    output$DataSummary2 <- renderTable({
      head(Table(),obs())
    })
    output$DataSummary3 <- renderTable({
      head(df3(),obs())
    })


    Model = reactive({
      eval(parse(text = paste0('M = lmer(',Formula(),', control=lmerControl(optimizer = "bobyqa"),',
                               'data=df3(), REML=FALSE)')))
      M
    })

    output$summary = renderTable({
      S = round(coef(summary(Model())),digits = 3)
      bind_cols(tibble(Effect = rownames(S)),
                as_tibble(S))
    })

    output$Anova = renderTable({
      M = anova(Model())
      bind_cols(tibble(Effect = rownames(M)),
                as_tibble(M))
    })

    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste("dataSummary.csv", sep = "")
      },
      content = function(file) {
        rio::export(SummaryTable(), file)
      }
    )

    output$downloadANOVA <- downloadHandler(
      filename = function() {
        paste("dataANOVA.csv", sep = "")
      },
      content = function(file) {
        rio::export(AnovaTable(), file)
      }
    )

    output$downloadfixpropdata = downloadHandler(
      filename = function(){
        paste('Data for plot.csv',sep = '')
      },
      content = function(file){
        rio::export(Table(), file)
      }
    )

    output$downloadanalysisdata = downloadHandler(
      filename = function(){
        paste('Data for analysis.csv',sep = '')
      },
      content = function(file){
        rio::export(df3(), file)
      }
    )

    output$Plot = renderPlot({
      if(isTRUE(Plot())){
        GrowthCurvePlot(df = df(),
                        Categories = Categories(),
                        OnsetName = OnsetName(),
                        rangemin = rangemin(),
                        rangemax = rangemax(),
                        steps = steps(), Xlab = Xlab(),Ylab = Ylab(),legendMain = Llab(),
                        Psize = Psize(),Textsize = Textsize(),FontFamily = FontFamily(),ColorVar = ColorVar(), FacetVar = FacetVar())

      }
    },width = function() return(Width()), height = function() return(Height()))
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
cat('########################\n6.You can run this command to perform survial analysis:\n
    SurvivalAnalysis_Shiny()\n\n########################\n\n')
cat('########################\n7.You can run this command to perform growth curve analysis:\n
    GrowthCurveAnalysis_shiny()\n\n########################\n\n')
cat('For more details and usages, please refer to the links below:\n
    https://zhuanlan.zhihu.com/p/68469202\n')
cat('\n########################\nPlease note that there will be continuous updates, so be sure to look out for it')
