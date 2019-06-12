
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(parallel)){install.packages('parallel')}
if(!require(shiny)){install.packages('shiny')}
if(!require(interactions)){install.packages('interactions')}
if(!require(lmerTest)){install.packages('lmerTest')}
if(!require(emmeans)){install.packages('emmeans')}
if(!require(jtools)){install.packages('jtools')}
if(!require(ggthemes)){install.packages('ggthemes')}

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
      M.Singular = isSingular(M)
      resulttable = data.frame(formula = Formulas[formula.id],
                               AIC = MAIC,
                               BIC = MBIC,
                               Singular = M.Singular)
      return(resulttable)
    }else{
      M = glmer(data = df, as.formula(Formulas[formula.id]), family = Family)
      MAIC = AIC(M)
      MBIC = BIC(M)
      M.Singular = isSingular(M)
      resulttable = data.frame(formula = Formulas[formula.id],
                               AIC = MAIC,
                               BIC = MBIC,
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
    Sys.time() - tic
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

        fileInput("file1", "Choose CSV File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),

        actionButton("update", "Update View")

      ),
      mainPanel(
        tableOutput("contents")
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

      d = read.csv(inFile$datapath, header = T)


      anovatable = eventReactive(input$update, {
        if(IVNumber() == 2){
          LMMRun_Parallel(df = d,
                          DV = DV(),
                          IV = c(IV1(), IV2()),
                          Cluster = c(Cluster1(), Cluster2()),
                          Manual = Manual(),Manualcodefilename = m, Family = FamilyD(),
                          Ifrun = Ifrun(),
                          Ncore = input$Ncore,
                          output = Output()) %>% arrange(Singular,BIC)
        }else{
          LMMRun_Parallel(df = d,
                          DV = DV(),
                          IV = c(IV1(), IV2(),IV3()),
                          Cluster = c(Cluster1(), Cluster2()),
                          Manual = Manual(),Manualcodefilename = m, Family = FamilyD(),
                          Ifrun = Ifrun(),
                          Ncore = input$Ncore,
                          output = Output()) %>% arrange(Singular,BIC)
        }}, ignoreNULL = FALSE)
      anovatable()
    })
  }

  print(shinyApp(ui, server))
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
        write.csv(formula_generate(DV = DV(),
                                   IV = c(IV1(), IV2()),
                                   Cluster = c(Cluster1(), Cluster2())) %>% as_tibble(), file, row.names = FALSE)
      }
    )


  }

  print(shinyApp(ui, server))
}

LMM_Model_Info_Shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY linear mixed model builder'),
    sidebarLayout(

      sidebarPanel(
        helpText('Model Building Part:'),
        textInput('Formula','Input the formula:',NULL),

        selectInput('Family', 'Select the distribution family of dependent variable:',
                    choices = c('gaussian','binomial','poisson')),

        selectInput('Contrasts','Select the type of contrasts:',
                    choices = c('sum','treatment')),

        fileInput("file1", "Choose CSV File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        numericInput("obs", "Set the number of observations to view:", 6),

        helpText('#######################'),
        helpText('Histogram on each Participants:'),
        checkboxInput('Split.Sub','Whether to plot histogram based on each subject?',F),
        sliderInput('NumCol','How many columns should the histogram be arranged?',min = 1, max = 20,step = 1,value = 3),
        textInput('SubName','Input the name of column indicating subject',NULL),
        textInput('DepenVar','Input the name of column indication dependent variable',NULL),

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
        checkboxInput('Plot','Whether to plot',F),
        selectInput('Geomtype','Select the geometry to draw',
                    choices = c('bar','line')),
        selectInput('Themes','Select the theme of plot:',
                    choices = c('origin','APA','Solar','Wall Street Journal')),
        selectInput('Color','Select the color palette',
                    choices = c('Set1','Set2','Set3')),
        textInput('Title','Input the title of plot:',NULL),
        textInput('Ylab','Input the label of y axis:', NULL),
        textInput('Xlab','Input the label of x axis:', NULL),
        textInput('LegendM','Input the title of legend:', NULL),
        sliderInput(inputId = 'LabelSize',label = 'Set the size of plot labels and title',min = 10, max = 50,step = 1, value = 10),
        checkboxInput('Dots','Whether draw raw data (dots)?',F)
      ),
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel('Data Summary',tableOutput("DataSummary")),
                    tabPanel('Model Summary',verbatimTextOutput("summary")),
                    tabPanel('Anova',tableOutput("Anova")),
                    tabPanel('Simple Effect',tableOutput('Emmeans'),tableOutput('Comparison')),
                    tabPanel('Plot', plotOutput('Plot',width = 600, height = 600)),
                    tabPanel('Sub.Histogram',plotOutput('Sub.Plot',width = 800,height = 800)))


      )
    )
  )

  server <- function(input, output) {

    Formula = reactive(input$Formula)

    Family = reactive(input$Family)

    obs = reactive(input$obs)

    Split.Sub = reactive(input$Split.Sub)
    NumCol = reactive(input$NumCol)
    SubName = reactive(input$SubName)
    DepenVar = reactive(input$DepenVar)

    SimpleEffect = reactive(input$SimpleEffect)
    IVNumber = reactive(input$IVNumber)
    Predictor = reactive(input$Predictor)
    Modulator1 = reactive(input$Modulator1)
    Modulator2 = reactive(input$Modulator2)

    PLOT = reactive(input$Plot)
    Geomtype = reactive(input$Geomtype)
    Themes = reactive(input$Themes)
    Color = reactive(input$Color)
    Dots = reactive(input$Dots)
    Title = reactive(input$Title)
    Ylab = reactive(input$Ylab)
    Xlab = reactive(input$Xlab)
    LegendM = reactive(input$LegendM)
    LabelSize = reactive(input$LabelSize)

    output$DataSummary = renderTable({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)
      head(d,n = obs())
    })

    output$summary = renderPrint({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)

      Contrasts = reactive({
        switch(input$Contrasts,
               'sum' = 'contr.sum',
               'treatment' = 'contr.treatment')
      })

      options(contrasts = c(Contrasts(),'contr.poly'))

      if(Family() %in% 'gaussian'){
        M = lmer(data = d,
                 formula = as.formula(Formula()))
        summary(M)
      }else{
        M = glmer(data = d,
                  formula = as.formula(Formula()),
                  family = Family())
        summary(M)
      }

    })
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste(input$DV,'Fixed_Effect_Table', ".csv", sep = "")
      },
      content = function(file) {
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = read.csv(inFile$datapath, header = T)

        if(Family() %in% 'gaussian'){
          M = lmer(data = d,
                   formula = as.formula(Formula()))
        }else{
          M = glmer(data = d,
                    formula = as.formula(Formula()),
                    family = Family())
        }
        M1 = round(summary(M)$coef,digits = 3)
        M1 = bind_cols(tibble(Effect = rownames(M1)),
                       as_tibble(M1))
        write.csv(M1, file, row.names = FALSE)
      }
    )
    output$Anova = renderTable({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)

      if(Family() %in% 'gaussian'){
        M = lmer(data = d,
                 formula = as.formula(Formula())) %>% anova()
        bind_cols(tibble(Effect = rownames(M)),
                  as_tibble(M))
      }else{
        M = glmer(data = d,
                  formula = as.formula(Formula()),
                  family = Family()) %>% anova()
        bind_cols(tibble(Effect = rownames(M)),
                  as_tibble(M))
      }
    })

    output$downloadAnova <- downloadHandler(

      filename = function() {
        paste(input$DV,'Anova_Table', ".csv", sep = "")
      },
      content = function(file) {
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = read.csv(inFile$datapath, header = T)

        if(Family() %in% 'gaussian'){
          M = lmer(data = d,
                   formula = as.formula(Formula()))
        }else{
          M = glmer(data = d,
                    formula = as.formula(Formula()),
                    family = Family())
        }
        M1 = round(anova(M),digits = 3)
        M1 = bind_cols(tibble(Effect = rownames(M1)),
                       as_tibble(M1))
        write.csv(M1, file, row.names = FALSE)
      }
    )

    output$Emmeans = renderTable({
      if(isTRUE(SimpleEffect())){
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = read.csv(inFile$datapath, header = T)
        if(Family() %in% 'gaussian'){
          M = lmer(data = d,
                   formula = as.formula(Formula()))
        }else{
          M = glmer(data = d,
                    formula = as.formula(Formula()),
                    family = Family())
        }
        if(IVNumber() == 2){
          eval(parse(text = paste0('emmeans(M, pairwise~',Predictor(),'|',Modulator1(),')$emm')))
        }else{
          eval(parse(text = paste0('emmeans(M, pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$emm')))
        }
      }

    })

    output$downloadEmmeans = downloadHandler(

      filename = function() {
        paste(input$DV,'Emmeans_Table', ".csv", sep = "")
      },
      content = function(file) {
        if(isTRUE(SimpleEffect())){
          inFile <- input$file1

          if (is.null(inFile))
            return(NULL)

          d = read.csv(inFile$datapath, header = T)
          if(Family() %in% 'gaussian'){
            M = lmer(data = d,
                     formula = as.formula(Formula()))
          }else{
            M = glmer(data = d,
                      formula = as.formula(Formula()),
                      family = Family())
          }
          options(digits = 3)
          if(IVNumber() == 2){
            eval(parse(text = paste0('M1 = emmeans(M, pairwise~',Predictor(),'|',Modulator1(),')$emm %>% as_tibble()')))
          }else{
            eval(parse(text = paste0('M1 = emmeans(M, pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$emm %>% as_tibble()')))
          }

        }
        write.csv(M1, file, row.names = FALSE)
      }
    )

    output$Comparison = renderTable({
      if(isTRUE(SimpleEffect())){
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = read.csv(inFile$datapath, header = T)
        if(Family() %in% 'gaussian'){
          M = lmer(data = d,
                   formula = as.formula(Formula()))
        }else{
          M = glmer(data = d,
                    formula = as.formula(Formula()),
                    family = Family())
        }
        if(IVNumber() == 2){
          eval(parse(text = paste0('emmeans(M, pairwise~',Predictor(),'|',Modulator1(),')$contr %>% as_tibble()')))
        }else{
          eval(parse(text = paste0('emmeans(M, pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$contr %>% as_tibble()')))
        }

      }
    })

    output$downloadComparison = downloadHandler(

      filename = function() {
        paste(input$DV,'Comparison_Table', ".csv", sep = "")
      },
      content = function(file) {
        if(isTRUE(SimpleEffect())){
          inFile <- input$file1

          if (is.null(inFile))
            return(NULL)

          d = read.csv(inFile$datapath, header = T)
          if(Family() %in% 'gaussian'){
            M = lmer(data = d,
                     formula = as.formula(Formula()))
          }else{
            M = glmer(data = d,
                      formula = as.formula(Formula()),
                      family = Family())
          }
          options(digits = 3)
          if(IVNumber() == 2){
            eval(parse(text = paste0('M1 = emmeans(M, pairwise~',Predictor(),'|',Modulator1(),')$contr %>% as_tibble()')))
          }else{
            eval(parse(text = paste0('M1 = emmeans(M, pairwise~',Predictor(),'|',Modulator1(),'|',Modulator2(),')$contr %>% as_tibble()')))
          }

        }
        write.csv(M1, file, row.names = FALSE)
      }
    )

    output$Plot = renderPlot({
      if(isTRUE(PLOT())){
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = read.csv(inFile$datapath, header = T)
        if(Family() %in% 'gaussian'){
          M = lmer(data = d,
                   formula = as.formula(Formula()))
        }else{
          M = glmer(data = d,
                    formula = as.formula(Formula()),
                    family = Family())
        }
        if(IVNumber() == 2){
          eval(parse(text = paste0('interactions::cat_plot(model = M, pred = ',Predictor(),', ',
                                   'modx = ',Modulator1(),', ',
                                   'geom = ','\'',Geomtype(),'\'',', ',
                                   'errorbar.width = 0.2,',
                                   'legend.main = \'',LegendM(),'\',',
                                   ifelse(Geomtype() %in% 'bar','','dodge.width = 0.3,'),
                                   'point.alpha = 0.1,',
                                   'colors = \'', Color(),'\',',
                                   'plot.points = ',Dots(),')',
                                   ifelse(Themes() %in% 'origin',
                                          '',
                                          ifelse(Themes() %in% 'APA',
                                                 '+jtools::theme_apa()',
                                                 ifelse(Themes() %in% 'Solar',
                                                        '+ggthemes::theme_solarized()',
                                                        '+ggthemes::theme_wsj()'))),
                                   '+labs(y = Ylab(), x = Xlab(), title = Title())',
                                   ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSize()+5,'),',
                                   ' axis.title.x = element_text(size = ',LabelSize(),
                                   '), axis.title.y = element_text(size = ',LabelSize(),
                                   '), legend.text = element_text(size = ',LabelSize()-5,
                                   '), legend.title = element_text(size = ',LabelSize(),
                                   '), axis.text.y = element_text(size = ',LabelSize()-5,
                                   '), axis.text.x = element_text(size = ',LabelSize()-5,'))')))
        }else{
          eval(parse(text = paste0('interactions::cat_plot(model = M, pred = ',Predictor(),', ',
                                   'modx = ',Modulator1(),', ',
                                   'mod2 = ',Modulator2(),', ',
                                   'geom = ','\'',Geomtype(),'\'',', ',
                                   'errorbar.width = 0.2,',
                                   'legend.main = \'',LegendM(),'\',',
                                   ifelse(Geomtype() %in% 'bar','','dodge.width = 0.3,'),
                                   'point.alpha = 0.1,',
                                   'colors = \'', Color(),'\',',
                                   'plot.points = ',Dots(),')',
                                   ifelse(Themes() %in% 'origin',
                                          '',
                                          ifelse(Themes() %in% 'APA',
                                                 '+jtools::theme_apa()',
                                                 ifelse(Themes() %in% 'Solar',
                                                        '+ggthemes::theme_solarized()',
                                                        '+ggthemes::theme_wsj()'))),
                                   '+labs(y = Ylab(), x = Xlab(), title = Title())',
                                   ' + theme(plot.title = element_text(hjust = 0.5, size = ',LabelSize()+5,'),',
                                   ' axis.title.x = element_text(size = ',LabelSize(),
                                   '), axis.title.y = element_text(size = ',LabelSize(),
                                   '), legend.text = element_text(size = ',LabelSize()-5,
                                   '), legend.title = element_text(size = ',LabelSize(),
                                   '), axis.text.y = element_text(size = ',LabelSize()-5,
                                   '), axis.text.x = element_text(size = ',LabelSize()-5,'))')))
        }
      }
    })

    output$Sub.Plot = renderPlot({
      if(isTRUE(Split.Sub())){
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        d = read.csv(inFile$datapath, header = T)
        Density.Sub = function(df, Sub, DV, NumCol){
          eval(parse(text = paste0('df$',Sub,' = factor(df$',Sub,')')))
          eval(parse(text = paste0(' p = ggplot(data = df, aes(x = ',DV,', fill = ',Sub,'))+geom_density()')))
          eval(parse(text = paste0('p = p + facet_wrap(~',Sub,', ncol = ',NumCol,')')))
          eval(parse(text = paste('p + labs(x = \'',Sub,'\',', y = '\'',DV,'\')')))
        }
        p = Density.Sub(df = d, Sub = SubName(),DV = DepenVar(),NumCol = NumCol())
        print(p)
      }
    })

  }

  print(shinyApp(ui, server))
}

Datafilter = function(NGroup,df, DV,
                      Group1=NULL, Group2=NULL, Group3=NULL, Group4 = NULL, Group5 = NULL, ZV = 3){
  if(NGroup == 0){
    eval(parse(text = paste0('df2 = df %>% mutate(Zvalue = scale(',DV,'))',' %>% ',
                             'filter(abs(Zvalue) < ',ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 1){
    eval(parse(text = paste0('df2 = df %>% group_by(',Group1,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 2){
    eval(parse(text = paste0('df2 = df %>% group_by(',Group1,', ',Group2,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 3){
    eval(parse(text = paste0('df2 = df %>% group_by(',Group1,', ',Group2,', ',Group3,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 4){
    eval(parse(text = paste0('df2 = df %>% group_by(',Group1,', ',Group2,', ',Group3,', ',Group4,')',' %>% ',
                             'mutate(Zvalue = scale(',DV,'))', ' %>% ',
                             'filter(abs(Zvalue) < ', ZV,')',' %>% ',
                             'select(-Zvalue)')))
  }

  if(NGroup == 5){
    eval(parse(text = paste0('df2 = df %>% group_by(',Group1,', ',Group2,', ',Group3,', ',Group4,', ',Group5,')',' %>% ',
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
        fileInput("file1", "Choose CSV File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),

        textInput('DV','Input the dependent variable:',NULL),

        selectInput('NumGroup','Select the number of categories',choices = c(0:5)),

        textInput('G1','Input the 1st factor:', NULL),

        textInput('G2','Input the 2nd factor:', NULL),

        textInput('G3','Input the 3rd factor:', NULL),

        textInput('G4','Input the 4th factor:', NULL),

        textInput('G5','Input the 5th factor:', NULL),

        sliderInput('ZV','Set the Z value to filter',min = 1, max = 5,step = 0.1, value = 3),

        numericInput("obs", "Set the number of observations to view:", 6),

        downloadButton("downloadData", "Download the formulas")
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

      d = read.csv(inFile$datapath, header = T)
      head(d,n = obs())
    })

    output$OldLine = renderText({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)
      print(paste0('There are ', nrow(d),' lines.'))
    })

    output$DataFiltered = renderTable({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)
      df = Datafilter(NGroup = NumGroup(), df = d,
                      DV = DV(),Group1 = G1(),Group2 = G2(),Group3 = G3(),Group4 = G4(),Group5 = G5(),
                      ZV = ZV())
      head(df, n = obs())
    })

    output$NewLine = renderText({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)
      df = Datafilter(NGroup = NumGroup(), df = d,
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

        d = read.csv(inFile$datapath, header = T)
        df = Datafilter(NGroup = NumGroup(), df = d,
                        DV = DV(),Group1 = G1(),Group2 = G2(),Group3 = G3(),Group4 = G4(),Group5 = G5(),
                        ZV = ZV())
        write.csv(df, file, row.names = FALSE)
      }
    )


  }

  print(shinyApp(ui, server))
}

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
cat('For more details and usages, please refer to the links below:\n
    https://zhuanlan.zhihu.com/p/67680257\n
    https://zhuanlan.zhihu.com/p/67048151\n
    https://zhuanlan.zhihu.com/p/63092231\n')
cat('\n########################\nPlease note that there will be continuous updates, so be sure to look out for it')
