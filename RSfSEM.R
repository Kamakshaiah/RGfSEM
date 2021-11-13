library(shiny)
library(shinydashboard)        
library(nortest)
library(mvnormtest)
library(MASS)
library(lavaan)
library(psych)
library(sem)


ui <- fluidPage(
  
  navbarPage(title = "RGfSEM",
             tabPanel("Data Sets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          radioButtons("indata", "Choice:", choices = c("Full", "Columns")),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                          downloadButton('downloaddatset', "Download"),
                          hr(),
                          radioButtons("trans1", "Transformation:", choices = c("Not-Required", "log", "inverselog", "exponential", "lognormal", "standardize")),
                          hr()
                          
                        ), 
                        
                        mainPanel(tableOutput("tab1"))
                      )
                      
             ), 
             
             navbarMenu("Statistical Analysis",
                        tabPanel("Summary Statistics",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE)
                                     
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Summary Statistics"),
                                       div(
                                         verbatimTextOutput("summar")
                                       )
                                     )
                                   )
                                 )
                        ), 
                        tabPanel("Frequency Tables",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols2", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols3", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE)
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Frequency Tables"),
                                       div(
                                         verbatimTextOutput("freq_tables")
                                       )
                                     )
                                   )
                                 )
                                 
                        ), 
                        
                        tabPanel("Plots",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotoption", "Choose the Option:", choices = c("Histogram", "BarPlot", "Scatter", "Pie" )),
                                     selectInput("cols6", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     textInput("xaxisname", "Write X Axis Name"),
                                     textInput("yaxisname", "Write Y Axis Name"),
                                     textInput("title", "Write Title For the Graph")
                                   ), 
                                   mainPanel(
                                     h3("Plots"),
                                     fluidRow(
                                       plotOutput("plot")
                                     )
                                   )
                                 )
                                 
                        ),
                        
                        tabPanel("Statistical Tests", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols7", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols8", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("normaltest", "Select Method:", choices = c("A-D-Test", "Shapiro", "KS-Test", "MV-Shapiro")),
                                     hr(),
                                     helpText("For more details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test", "Anderson–Darling test"), br(),
                                     a(href="https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test", "Shapiro–Wilk test"), br(),
                                     a(href="https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test", "Kolmogorov–Smirnov test"), br(),
                                     
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Statistical Tests"),
                                     fluidRow(
                                       div(
                                         plotOutput("qqp")
                                       ),
                                       div(
                                         verbatimTextOutput("normtest")
                                       )
                                     )
                                   )
                                   
                                 )
                        ),
                        
                        tabPanel("Correlation", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols9", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols10", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("cormethod", "Select Method:", choices = c("Covariance", "KarlPearson", "Spearman", "Kendals")),
                                     hr(),
                                     helpText("For Details Visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Covariance & Correlation"),
                                     verbatimTextOutput("cor_t")
                                   )
                                   
                                 )
                                 
                        ),
                        
                        tabPanel("Regression & ANOVA", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols11", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols12", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("regmethod", "Select Method:", choices = c("Fit", "Summary", "ANOVA")), 
                                     hr(),
                                     helpText("For more information please visit"),
                                     a(href="https://en.wikipedia.org/wiki/Simple_linear_regression", "Simple Linear Regression"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Regression & ANOVA"),
                                     fluidRow(
                                       div(
                                         verbatimTextOutput("regout")
                                       ),
                                       div(
                                         plotOutput("regplot")
                                       )
                                     )
                                   )
                                   
                                 )
                                 
                        )
                        
             ),
             
             navbarMenu("SEM",
                        
                        tabPanel("EFA & SEM",
                                 sidebarLayout(
                                   sidebarPanel(
                                     # selectInput("cols13", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     hr(),
                                     helpText("Entire data set is used for fitting EFA; Make data convenient well before uploading at 'Data Sets' menu."),
                                     hr(),
                                     
                                     textInput("nf1", "No. of Factors", value = "2", placeholder = "Write the number"),
                                     radioButtons("efaplot", "Structure:", choices = c("EFA", "Structure Diagram", "I don't know anything; please fit EFA")),
                                     radioButtons("efaomegaplot", "Structural Plots", choices = c("No Plot", "Omega Plot")), 
                                     helpText("Read details about EFA & SEM at"),
                                     a(href="https://personality-project.org/r/book/psych_for_sem.pdf", "psych for SEM")
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       h3("EFA & SEM"),
                                       hr(),
                                       div(
                                         verbatimTextOutput("efaoutput")
                                       ),
                                       div(
                                         plotOutput("efadiagram")
                                       )
                                     )
                                   )
                                 )
                          ),
                        tabPanel("CFA & SEM",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Entire data set is used for fitting SEM; Make data convenient well before uploading at 'Data Sets' menu."),
                                     fileInput("lavmod", "Upload Model"),
                                     helpText("You need to upload model defined in text file with a format 'mymodel.lav'"),
                                     textInput("nf2", "No. of Factors", value = "3", placeholder = "Write the number"),
                                     radioButtons("semtype", "Structure:", choices = c("CFA", "CFA Summary", "Fit Measures", "I don't know anything; please fit SEM")),
                                     radioButtons("semplot", "Structural Plots", choices = c("No Plot", "Structure Diagram", "Omega Plot")),
                                     helpText("Read more about CFA & SEM at"),
                                     a(href="http://lavaan.ugent.be/tutorial/tutorial.pdf", "lavaan for SEM")
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       h3("CFA & SEM"),
                                       hr(),
                                       div(
                                         verbatimTextOutput("semoutput")
                                       ),
                                       div(
                                         plotOutput("semdiagram")
                                       )
                                     )
                                   )
                                 )
                          )
                        ),
             tabPanel("Contact", 
                      sidebarLayout(
                        sidebarPanel(
                          "Information to contact"
                        ), 
                        mainPanel(htmlOutput("text1"))
                      )
             )
  )
)





server <- function(input, output, session) {
  
  # for DATASET TAB
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session, inputId = "cols", choices = names(data_input()))
  }
  )
  
  logno <- reactive({
    df <- data_input()
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    for(i in 1:length(df[, input$cols])){
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- dlnorm(df[, input$cols][[i]][j]) 
      }
    }
    return(t(x))
  })
  
  standout <- reactive({
    df <- data_input()
    
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    
    if(!is.list(df[, input$cols])){
      df[, input$cols] <- list(df[, input$cols])
    }
    
    for(i in 1:length(df[, input$cols])){
      
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- df[, input$cols][[i]][j]-mean(df[, input$cols][[i]])/sd(df[, input$cols][[i]])
      }
    }
    return(t(x))
    
  })
  
  output$tab1 <- renderTable(
    {
      df <- data_input()
      
      if (input$indata == "Full"){
        print(df)
      } else if(input$trans1 == "Not-Required"){
        data <- df[, input$cols]
        print(data)
      } else if(input$trans1 == "log"){
        data <- log(df[input$cols])
        print(data)
      } else if(input$trans1 == "inverselog"){
        data <- 1/log(df[input$cols])
        print(data)
      } else if(input$trans1 == "exponential"){
        data <- exp(df[input$cols])
        print(data)
      } else if(input$trans1 == "lognormal"){
        logno()
      } else if(input$trans1 == "standardize"){
        standout()
      }
      
    }
  )
  
  
  output$downloaddatset <- downloadHandler(
    
    filename <- function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    
    content <- function(file){
      df <- data_input()
      write.csv(df[, input$cols], file, row.names = TRUE)
    }
    
  )
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
  }
  )
  
  summ <- reactive({
    var1 <- data_input()[,input$cols1]
    
    su <- summary(var1)
    return(su)
  })
  
  output$summar <- renderPrint({
    summ()
  })
  
  # frequency tab
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols2", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols3", choices = names(data_input()))
  }
  )
  
  freq <- reactive({
    var1 <- data_input()[,input$cols2]
    var2 <- data_input()[,input$cols3]
    fre <- table(var1, var2)
    return(fre)
  })
  
  output$freq_tables <- renderPrint({
    freq()
  })
  
  # Cross tabulation
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols4", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols5", choices = names(data_input()))
  }
  )
  
  cross <- reactive({
    var1 <- data_input()[,input$cols4]
    var2 <- data_input()[,input$cols5]
    
    cro <- chisq.test(var1, var2)
    return(cro)
  })
  
  output$chi_t <- renderPrint({
    cross()
    
  })
  
  # Plots 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols6", choices = names(data_input()))
  }
  )
  
  output$plot <- renderPlot({
    df <- data_input()
    if(input$plotoption == "Histogram"){
      hist(df[, input$cols6], freq = FALSE, xlab = input$xaxisname, ylab = input$yaxisname, main = input$title); lines(density(df[, input$cols6]), col = "red", lwd = 1.5)
    } else if(input$plotoption == "BarPlot"){
      barplot(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else if(input$plotoption == "Scatter"){
      scatter.smooth(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else {
      pie(table(df[, input$cols6]))
    }
  })
  
  # Statistical Tests
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols7", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols8", choices = names(data_input()))
  }
  )
  
  output$qqp <- renderPlot({
    df <- data_input()
    qqnorm(df[, input$cols7]);qqline(df[, input$cols7])
  })
  
  adt <- reactive({
    df <- data_input()
    var <- df[, input$cols7]
    ad <- ad.test(var)
    return(ad)
  })
  
  sht <- reactive({
    df <- data_input()
    var <- df[, input$cols7]
    sh <- shapiro.test(var)
    return(sh)
  })
  
  kst <- reactive({
    df <- data_input()
    var1 <- df[, input$cols7]
    var2 <- df[, input$cols8]
    ks <- ks.test(var1, var2)
    return(ks)
  })
  
  mvst <- reactive({
    df <- data_input()
    var1 <- df[, input$cols7]
    var2 <- df[, input$cols8]
    return(mshapiro.test(t(as.data.frame(var1, var2))))
  })
  
  output$normtest <- renderPrint({
    
    if(input$normaltest == "A-D-Test"){
      print(adt())
    } else if(input$normaltest == "Shapiro"){
      print(sht())
    } else if(input$normaltest == "KS-Test"){
      print(kst())
    } else if(input$normaltest == "MV-Shapiro"){
      print(mvst())
    }
    
  }
  
  )
  # correlation & regression 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols9", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols10", choices = names(data_input()))
  }
  )
  
  cortest <- reactive({
    var1 <- data_input()[,input$cols9]
    var2 <- data_input()[,input$cols10]
    
    if (input$cormethod == "Covariance"){
      return(cov(var1, var2))
    } else if (input$cormethod == "KarlPearson"){
      return(cor.test(var1, var2, method = "pearson"))
    } else if(input$cormethod == "Spearman"){
      return(cor.test(var1, var2, method="spearman"))
    } else {
      return(cor.test(var1, var2, method="kendall"))
    }
  }
  )
  
  output$cor_t <- renderPrint({
    
    cortest()
  })
  
  # simple linear regression
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols11", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols12", choices = names(data_input()))
  }
  )
  
  lmout <- reactive({
    df <- data_input()
    var1 <- df[, input$cols11]
    var2 <- df[, input$cols12]
    out <- lm(var1 ~ var2, data = df)
    return(out)
  })
  
  output$regout <- renderPrint({
    if(input$regmethod == "Fit"){
      lmout()
    } else if(input$regmethod == "Summary"){
      summary(lmout())
    } else if (input$regmethod == "ANOVA"){
      anova(lmout())
    }
  })
  
  output$regplot <- renderPlot({
    par(mfrow=c(2,2))
    plot(lmout())
  })
  # efa & sem
  
  faout <- reactive({
    
    df <- data_input()

    fit <- fa(df, input$nf1)
    return(fit)
  }
    
  )
  
  omegaout <- reactive({
    df <- data_input()
    fit <- omega(df)
    return(fit)
  })
  
  output$efaoutput <- renderPrint({
    if(input$efaplot == "I don't know anything; please fit EFA"){
      omegaout()
    } else {
      faout()
    }
    
  })
  
  output$efadiagram <- renderPlot({
    if(input$efaplot == "Structure Diagram"){
      structure.diagram(faout())
    } else if(input$efaomegaplot == "Omega Plot"){
      omega.diagram(omegaout())
    }
  })
  

  # CFA & SEM
  
  model <- reactive({
    infile <- input$lavmod
    req(infile)
    readLines(infile$datapath) 
  })
  
  cfaout <- reactive({
    df <- data_input()
    
    model <- model()
    fit <- lavaan::sem(model, data = df)
    return(fit)
  })
  
  omegasem <- reactive({
    df <- data_input()
    fafit <- fa(df, input$nf2)
    ss <- structure.sem(fafit)
    semfit <- cfaout()
    return(omegaFromSem(semfit, cor(df)))
    
  
  })
  
  output$semoutput <- renderPrint({
    if(input$semtype == "CFA"){
      cfaout()
    } else if(input$semtype == "I don't know anything; please fit SEM"){
      omegasem()
    } else if(input$semtype == "CFA Summary"){
      summary(cfaout())
    } else if(input$semtype == "Fit Measures"){
      as.data.frame(fitMeasures(cfaout()))
    }
    
  })
    
  output$semdiagram <- renderPlot({
    if(input$semplot == "Structure Diagram"){
      lavaan.diagram(cfaout())
    } else if(input$semplot == "Omega Plot"){
      omega.diagram(omegasem())
    }
    
  })
  
  
  # Contact Information 
  
  output$text1 <- renderText({
    str1 <- paste("Dr. M. Kamakshaiah") 
    str2 <- paste("kamakshaiah.m@gmail.com") 
    str3 <- paste("+919177573730")
    #str4 <- paste("166, Vayushakti Nagar, Dammaiguda, Hyderabad, Telangana State, India 500083")
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
}



shinyApp(ui, server)
