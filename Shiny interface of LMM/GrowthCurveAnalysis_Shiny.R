rm(list = ls())
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
                           Xlab, Ylab, legendMain,Psize,Textsize){
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


  eval(parse(text = paste0('ggplot(data = dfNew3, aes(x = Time, y = FixProp, color = ',Category[[length(Category)]],' %>% factor()))+',
                           'geom_point(size = Psize)+ geom_line(aes(group = ',Category[[length(Category)]],'))+',
                           'labs(x = Xlab, y = Ylab, color = legendMain)+',
                           'scale_color_brewer(palette = \'Set1\')',
                           ifelse(length(Category)>2,
                                  paste0('+facet_wrap(~',Category[[2]],', ncol=1)'),
                                  ''),
                           '+theme(axis.text = element_text(size = ',Textsize-3,'),',
                           'axis.title = element_text(size = ',Textsize,'),',
                           'legend.text = element_text(size = ',Textsize-3,'),',
                           'legend.title = element_text(size = ',Textsize,'))')))
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

        checkboxInput('Plot',label = 'Whether to plot growth curve',value = F),
        textInput('Xlab', 'Input the x axis label:',NULL),
        textInput('Ylab', 'Input the y axis label:',NULL),
        textInput('Llab', 'Input the legend main:',NULL),
        numericInput(inputId = 'Psize',label = 'Set the size of point',value = 0.5,step = 0.01),
        numericInput(inputId = 'Textsize',label = 'Set the size of text',value = 15,step = 0.1),
        numericInput(inputId = 'Width',label = 'Set the plot Width',value = 400,step = 1),
        numericInput(inputId = 'Height',label = 'Set the plot Height',value = 400,step = 1),

        downloadButton("downloadSummary", "Download the Summary"),
        downloadButton("downloadANOVA", "Download the ANOVA"),
        downloadButton("downloadfixpropdata", "Download the fix prop data")
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
    Plot = reactive(input$Plot)
    Xlab = reactive(input$Xlab)
    Ylab = reactive(input$Ylab)
    Llab = reactive(input$Llab)
    Psize = reactive(input$Psize)
    Textsize = reactive(input$Textsize)
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
        Time = poly(unique(df$TimeBins),4)
        df[,paste0('ot',1:4)] = Time[df$TimeBins - min(df$TimeBins)+1,1:4]
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
        paste('FixPropData.csv',sep = '')
      },
      content = function(file){
        rio::export(Table(), file)
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
                        Psize = Psize(),Textsize = Textsize())
      }
    },width = function() return(Width()), height = function() return(Height()))
  }

  print(shinyApp(ui, server))
}
