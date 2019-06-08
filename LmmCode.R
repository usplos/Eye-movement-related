
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(parallel)){install.packages('parallel')}
if(!require(shiny)){install.packages('shiny')}

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
    titlePanel('Shiny version of LmmCode_Parallel'),
    sidebarLayout(

      sidebarPanel(
        textInput('DV','Input DV:','Y'),

        selectInput('IVNumber','Select the number of IVs',choices = c(2,3)),

        textInput('IV1','Input IV1:', 'A'),

        textInput('IV2','Input IV2:', 'B'),

        textInput('IV3','Input IV3:', 'C'),

        textInput('Cluster1','Input Cluster1:', 'Sub'),

        textInput('Cluster2','Input Cluster2:', 'Item'),

        checkboxInput('Ifrun','Ifrun',T),

        checkboxInput('Manual','Manual',F),

        textInput('mfile', 'Input formula file name',NULL),

        selectInput('Family', 'Input the distribution family',
                    choices = c('gaussian','binomial','poisson')),

        sliderInput('Ncore','Number of Cores', min = 1, max = 20, value = 4, step = 1),

        textInput('Output', 'Ouput file prefix name:','Y'),

        fileInput("file1", "Choose CSV File",
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
    titlePanel('Shiny version of formula_generate'),
    sidebarLayout(

      sidebarPanel(
        textInput('DV','Input DV:','Y'),

        selectInput('IVNumber','Select the number of IVs:',choices = c(2,3)),

        textInput('IV1','Input IV1:', 'A'),

        textInput('IV2','Input IV2:', 'B'),

        textInput('IV3','Input IV3:', 'C'),

        textInput('Cluster1','Input Cluster1:', 'Sub'),

        textInput('Cluster2','Input Cluster2:', 'Item'),

        downloadButton("downloadData", "Download")
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
    titlePanel('SHINY Graphic user interfere of linear mixed model'),
    sidebarLayout(
      
      sidebarPanel(
        textInput('Formula','Input the formula:',NULL),
        
        selectInput('Family', 'Input the distribution family',
                    choices = c('gaussian','binomial','poisson')),
        
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        numericInput("obs", "Number of observations to view:", 6)
      ),
      mainPanel(
        h4("Data summary:"),
        tableOutput("DataSummary"),
        h4("Model summary:"),
        verbatimTextOutput("summary"),
        h4("Anova:"),
        tableOutput("Anova")
      )
    )
  )
  
  server <- function(input, output) {
    
    Formula = reactive(input$Formula)
    
    Family = reactive(input$Family)
    
    obs = reactive(input$obs)
    
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
      
      if(Family() %in% 'gaussian'){
        lmer(data = d,
             formula = as.formula(Formula())) %>% summary()
      }else{
        glmer(data = d,
              formula = as.formula(Formula()),
              family = Family()) %>% summary()
      }
      
      })
    output$Anova = renderTable({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      d = read.csv(inFile$datapath, header = T)
      
      if(Family() %in% 'gaussian'){
        lmer(data = d,
             formula = as.formula(Formula())) %>% anova()
      }else{
        glmer(data = d,
              formula = as.formula(Formula()),
              family = Family()) %>% anova()
      }
    })
    
  }
  
  print(shinyApp(ui, server))
}
