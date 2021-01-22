library(baseline)
library(caret)
library(chillR)
library(dashboardthemes)
library(DT)
library(e1071)
library(EMSC)
library(ggplot2)
library(magrittr)
library(mdatools)
library(plsVarSel)
library(prospectr)
library(shinycssloaders)
library(shinydashboard)
library(shinyFiles)


## DATA INPUT MODULE----------------------------------------------------------------
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label)
  )
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.table(userFile()$datapath,
               header = TRUE)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

### creating custom theme object
theme_poor_mans_flatly <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(33,37,41)"
  ,primaryFontColor = "rgb(245,245,245)"
  ,infoFontColor = "rgb(245,245,245)"
  ,successFontColor = "rgb(33,37,41)"
  ,warningFontColor = "rgb(33,37,41)"
  ,dangerFontColor = "rgb(33,37,41)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(24,188,156)"
  
  ,headerButtonBackColor = "rgb(24,188,156)"
  ,headerButtonIconColor = "rgb(44,62,80)"
  ,headerButtonBackColorHover = "rgb(20,154,128)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(24,188,156)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(44,62,80)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "inherit"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(44,62,80)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(30,43,55)"
  ,sidebarTabTextColorSelected = "rgb(24,188,156)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(44,62,80)"
  ,sidebarTabTextColorHover = "rgb(24,188,156)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 10
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 19
  ,boxDefaultColor = "rgb(52,152,219)"
  ,boxPrimaryColor = "rgb(44,62,80)"
  ,boxInfoColor = "rgb(52,152,219)"
  ,boxSuccessColor = "rgb(0, 179, 89)"
  ,boxWarningColor = "rgb(243,156,18)"
  ,boxDangerColor = "rgb(231,76,60)"
  
  ,tabBoxTabColor = "rgb(44,62,80)"
  ,tabBoxTabTextSize = 16
  ,tabBoxTabTextColor = "rgb(24, 188, 156)"
  ,tabBoxTabTextColorSelected = "rgb(255, 255, 255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(255,255,255)"
  ,tabBoxBorderRadius = 10
  
  ### inputs
  ,buttonBackColor = "rgb(44,62,80)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(44,62,80)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(30,43,55)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(30,43,55)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(31, 166, 54)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(89,126,162)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(236,240,241)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)



ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = img(src = "logo.png", height = 51, width = 151,
                                                tags$head(
                                                  tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left}
        "))
                                                ))),
                    dashboardSidebar(
                      sidebarMenu(
                        id="tabs",
                        menuItem("DATA INPUT", tabName = "dashboard", icon = icon("bar-chart-o")),
                        menuItem("PRE-ANALYSIS", tabName = "ayar", icon = icon("cog", lib = "glyphicon")),
                        menuItem("POST-ANALYSIS", icon = icon("table"), tabName = "widgets"),
                        menuItem("ABOUT", tabName = "about", icon = icon("list-alt"))
                      )
                    ),
                    
                    ## Body content
                    dashboardBody(theme_poor_mans_flatly,
                                  tags$head(tags$style(HTML(".small-box {height: 80px}"))),          
                                  tabItems(
                                    # First tab content
                                    tabItem(tabName = "dashboard",
                                            fluidPage(
                                              style = "border: 6px solid green;border-radius: 15px",
                                              cellArgs = list(style = "padding: 6px"),
                                              fluidRow(style = "border: 4px double silver;",
                                                       column(width = 4, height = 465,style = "border: 4px double silver;border-radius: 15px",
                                                              # Dynamic valueBoxes
                                                              box(title = "SelectWave", width = NULL, status="success",solidHeader = TRUE, "A user-friendly app for on-line multivarite data analysis and modelling", align='left'),
                                                              csvFileInput("file1", "XCAL (.txt format)"),
                                                              csvFileInput("file2", "YCAL (.txt format)"),
                                                              csvFileInput("file3", "XVAL (.txt format)"),
                                                              csvFileInput("file4", "YVAL (.txt format)"),
                                                              actionButton("goButton", "Next!"),
                                                              br()),
                                                       
                                                       column(style = "border: 4px double silver;border-radius: 15px;",
                                                              width = 8,
                                                              tabBox(width = NULL,height = 540,
                                                                     tabPanel("Data Input", div(style = 'overflow-y:scroll;height:450px',
                                                                                                tableOutput(outputId = "table1")))
                                                              ))))),
                                    
                                    # Second tab content
                                    tabItem(tabName = "widgets",
                                            fluidPage(
                                              style = "border: 6px solid green;border-radius: 15px",
                                              cellArgs = list(style = "padding: 6px"),
                                              fluidRow(style = "border: 4px double silver;",
                                                       column(width = 4, height = 465,style = "border: 4px double silver;border-radius: 15px",
                                                              # Dynamic valueBoxes
                                                              box(title = "VARIABLE SELECTION", width = NULL, status="success",solidHeader = TRUE,
                                                                  textInput("text", "SELECTION NAME:")),
                                                              box(title = "POST ANALYSIS", width = NULL, status="success",solidHeader = TRUE,
                                                                  # Post-analysis
                                                                  fluidRow(
                                                                    column(8,
                                                                           selectInput("select_sel", ("Variable selection"),
                                                                                       choices = list("None" = 1, "Filter methods" = list("VIP" = 2, "SR" = 3, "sMC" = 4, "mRMR" = 5), "Wrapper methods" = list("iPLS" = 6, "GA-PLS" = 7, "IPW-PLS" = 8, "UVE-PLS" = 9)), selected = 1),
                                                                    ),
                                                                    column(4,
                                                                           textInput("input_sel", label=("cut-off"), value=0))
                                                                  ),
                                                                  fluidRow(
                                                                    column(4,
                                                                           selectInput("select_meth", ("Analysis 2"),
                                                                                       choices = list("PLS" = 1, "SVM" = 2), selected = 1),
                                                                    ),
                                                                    column(4,
                                                                           textInput("input_meth", label=("#comp."), value=10)),
                                                                    column(4,
                                                                           textInput("input_meth_cv", label=("CV"), value="10%"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(4,
                                                                           actionButton("btn_analyse", "Apply")),
                                                                    column(8,
                                                                           actionButton("btn_plot", "Prediction plot"))
                                                                  )),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br()),
                                                       
                                                       column(style = "border: 4px double silver;border-radius: 15px;",
                                                              width = 8,
                                                              tabBox(width = NULL,height = 550,
                                                                     tabPanel("Results Table", DT::dataTableOutput(outputId = "Results2")),
                                                                     tabPanel("Prediction Plots", plotOutput("plot2post", height = 400)),
                                                                     tabPanel("Selection Plot", plotOutput(outputId = "plot_sel"))
                                                              ))))),
                                    # Second tab content
                                    tabItem(tabName = "ayar",
                                            fluidPage(
                                              style = "border: 6px solid green;border-radius: 15px",
                                              cellArgs = list(style = "padding: 6px"),
                                              fluidRow(style = "border: 4px double silver;",
                                                       column(width = 4, height = 460,style = "border: 4px double silver;border-radius: 15px",
                                                              box(title = "PRE-TREATMENT", width = NULL, status="success",solidHeader = TRUE,
                                                                  # Derivatives
                                                                  fluidRow(
                                                                    column(8,
                                                                           selectInput("select_deriv", ("Derivatives"),
                                                                                       choices = list("None" = 1, "First" = 2, "Second" = 3, "Smoothing" = 4), selected = 1),
                                                                    ),
                                                                    column(4,
                                                                           textInput("input_deriv", label=("points"), value=11))
                                                                  ),
                                                                  # Pretreatment
                                                                  fluidRow(
                                                                    column(8,
                                                                           selectInput("select_pre", ("Pretreatment"),
                                                                                       choices = list("None" = 1, "SNV" = 2, "MSC" = 3, "EMSC" = 4, "Baseline, poly." = 5), selected = 1),
                                                                    ),
                                                                    column(4,
                                                                           textInput("input_pre", label=("degree"), value=0))
                                                                  ),
                                                                  actionButton("btn_spec_plot", "Spectral plot")),
                                                              box(title = "PRE-ANALYSIS", width = NULL, status="success",solidHeader = TRUE,
                                                                  # Pre-analysis
                                                                  fluidRow(
                                                                    column(4,
                                                                           selectInput("select_meth_pre", ("Analysis 1"),
                                                                                       choices = list("PLS" = 1), selected = 1),
                                                                    ),
                                                                    column(4,
                                                                           textInput("input_meth_pre", label=("#comp."), value=10)),
                                                                    column(4,
                                                                           textInput("input_meth_cv_pre", label=("CV"), value="10%"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(4,
                                                                           actionButton("btn_analyse_pre", "Apply")),
                                                                    column(8,
                                                                           actionButton("btn_plot_pre", "Prediction plot"))
                                                                  )),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              actionButton("goButton1", "Next!"),
                                                              br()),
                                                       
                                                       column(style = "border: 4px double silver;border-radius: 15px;",
                                                              width = 8,
                                                              tabBox(width = NULL,height = 550,
                                                                     tabPanel("Spectral Plot", plotOutput("plot1", height = 400)),
                                                                     tabPanel("Results Table", DT::dataTableOutput(outputId = "Results1")),
                                                                     tabPanel("Prediction Plots", plotOutput("plot2", height = 400))
                                                              ))))),
                                    # Second tab content
                                    tabItem(tabName = "about",
                                            box(width = 12, title ="About Selectwave",height= 550,status="success",solidHeader = TRUE,
                                                p("The SelectWave application is developed using the shiny package under the RStudio platform."),
                                                br(),
                                                p("For more detailed information, please visit",
                                                  tags$a("BAF Elektronik Yazilim Tarim Inc.",
                                                         href = "http://www.baf-eyt.com.tr")),
                                                br(),
                                                p("Reference   => SelectWave: A Graphical User Interface for Wavelength Selection and Spectral Data Analysis."),
                                                p("Developers  => Dr Fatih Kahriman and Dr Kristian Hovde Liland"),
                                                p("User Manual =>",
                                                  tags$a("Link 1 = Direct link",
                                                         href = "UserManual.pdf", download=NA, target="_blank")),
                                                p("User Manual =>",
                                                  tags$a("Link 2 = GitHub repository",
                                                         href = "https://github.com/fkahriman/SelectWave/blob/master/inst/docs/UserManual.pdf")),
                                                h3("Specialities:"),
                                                p("- Open-source, GNU licenced app."),
                                                p("- Requires no code writing."),
                                                p("- Well-known multivariate modelling techniques."),
                                                p("- Developed for scientific studies."))
                                    )
                                  )
                    )
)

server <- function(input, output,session) {
  
  ####OPENING######################################
  showModal(modalDialog(
    title = "Important message",
    "If you are a first time user, please create mandatory files via click create mandatory files button!",
    footer = tagList(
      actionButton("btn4", "Create mandatory files"),
      modalButton("OK")
    )
  ))
  
  observeEvent(input$btn4, {
    # Create required files for first time users
    df <- data.frame("Variable" = "", "Derivative" = "", "Pretreatment" = "","Variable Selection" = "", "RMSEC" = "", "R2Cal" = "", "RPDCal" = "", "RMSECV" = "", "R2CV" = "", "RPDcv" = "", "RMSEP" = "", "R2Val" = "", "RPDVal" = "", check.names = FALSE)
    New1 <- write.csv(df[-1,], file = "~/plstum.csv")
    df <- data.frame("Variable" = "", "Derivative" = "", "Pretreatment" = "","Variable Selection" = "", "RMSEC" = "", "R2Cal" = "", "RPDCal" = "", "RMSEP" = "", "R2Val" = "", "RPDVal" = "", check.names = FALSE)
    New2 <- write.csv(df[-1,], file = "~/svmtum.csv")
  })  
  
  ###### DATA MENU##################################
  
  XCAL <- callModule(csvFile, "file1",
                     stringsAsFactors = FALSE)
  YCAL <- callModule(csvFile, "file2",
                     stringsAsFactors = FALSE)
  XVAL <- callModule(csvFile, "file3",
                     stringsAsFactors = FALSE)
  YVAL <- callModule(csvFile, "file4",
                     stringsAsFactors = FALSE)
  
  output$table1 <- renderTable({
    head(dataXCAL())
  })
  
  observeEvent(input$goButton, {
    newtab <- switch(input$tabs,
                     "dashboard" = "ayar",
                     "ayar" = "dashboard"
    )
    updateTabItems(session, "tabs", newtab)
  })
  ###################PRE-ANALYSIS MENU##########################  
  model_pre_selection <- NULL
  selected_variables <- NULL
  last_model_run <- 0
  
  dataXCAL <- reactive(
    {
      par_deriv <- c(as.numeric(input$select_deriv), as.numeric(input$input_deriv))
      par_pre   <- c(as.numeric(input$select_pre),   as.numeric(input$input_pre))
      X <- as.matrix(XCAL())
      
      # Derivative / smoothing
      if(par_deriv[1] > 1){
        if(par_deriv[2]>ncol(X))
          par_deriv[2] <- ifelse(ncol(X)%%2==0, ncol(X)%%2-1, ncol(X)%%2)
        if(par_deriv[1] < 4){
          X <- savitzkyGolay(X, par_deriv[1]-1, 3, par_deriv[2])
          # X <- SavitzkyGolay(X, poly = 3, width = par_deriv[2], deriv = par_deriv[1]-1)
        } else {
          X <- savitzkyGolay(X, 0, 3, par_deriv[2])
          # X <- SavitzkyGolay(X, poly = 3, width = par_deriv[2], deriv = 0)
        }
      }
      
      # Pretreatment
      if(par_pre[1] > 1){
        if(par_pre[1] == 2){ # SNV
          Z <- t(scale(t(X)))
          dims <- dim(Z)
          attributes(Z) <- NULL
          Z <- matrix(Z, nrow=dims[1])
          dimnames(Z) <- dimnames(X)
          X <- Z
        }
        if(par_pre[1] == 3) # MSC
          X <- EMSC(X, degree=0)$corrected
        if(par_pre[1] == 4){
          X <- EMSC(X, degree=par_pre[2])$corrected
        }
        if(par_pre[1] == 5)
          X <- baseline.modpolyfit(X, degree = par_pre[2], tol = 0.001, rep = 100)$corrected
      }
      X
    }
  )
  
  dataXVAL <- reactive(
    {
      par_deriv <- c(as.numeric(input$select_deriv), as.numeric(input$input_deriv))
      par_pre   <- c(as.numeric(input$select_pre),   as.numeric(input$input_pre))
      X  <- as.matrix(XVAL())
      XC <- as.matrix(XCAL())
      
      # Derivative / smoothing
      if(par_deriv[1] > 1){
        if(par_deriv[2]>ncol(X))
          par_deriv[2] <- ifelse(ncol(X)%%2==0, ncol(X)%%2-1, ncol(X)%%2)
        if(par_deriv[1] < 4){
          X <- savitzkyGolay(X, par_deriv[1]-1, 3, par_deriv[2])
          XC <- savitzkyGolay(XC, par_deriv[1]-1, 3, par_deriv[2])
          # X <- SavitzkyGolay(X, poly = 3, width = par_deriv[2], deriv = par_deriv[1]-1)
        } else {
          X <- savitzkyGolay(X, 0, 3, par_deriv[2])
          XC <- savitzkyGolay(XC, 0, 3, par_deriv[2])
          # X <- SavitzkyGolay(X, poly = 3, width = par_deriv[2], deriv = 0)
        }
      }
      
      # Pretreatment
      if(par_pre[1] > 1){
        if(par_pre[1] == 2){ # SNV
          X <- t(scale(t(X)))
          dims <- dim(X)
          attributes(X) <- NULL
          X <- matrix(X, nrow=dims[1])
        }
        if(par_pre[1] == 3) # MSC
          X <- EMSC(X, degree=0, reference=colMeans(XC))$corrected
        if(par_pre[1] == 4){
          X <- EMSC(X, degree=par_pre[2], reference=colMeans(XC))$corrected
        }
        if(par_pre[1] == 5)
          X <- baseline.modpolyfit(X, degree = par_pre[2], tol = 0.001, rep = 100)$corrected
      }
      X
    }
  )
  
  
  observeEvent(input$btn_analyse_pre, {#input$btn2
    op1()# cat("Showing", input$file1, "rows\n")
  })
  
  
  # Change input field for pretreatment based on selection
  observeEvent(input$select_pre,{
    if(input$select_pre=="4"){
      updateTextInput(session, "input_pre", value = "2")
    } else {
      if(input$select_pre=="5"){
        updateTextInput(session, "input_pre", value = "4")
      } else {
        updateTextInput(session, "input_pre", value = "0")
      }
    }
  })
  
  # Analysis 1
  op1 <- eventReactive(req(input$btn_analyse_pre), { # input$btn2
    withProgress(message = 'Processing', detail = 'Reading (and pre-processing) data', value = 0, {
      datasetC <- data.frame(X = I(as.matrix(dataXCAL())), Y=YCAL()[[1]])
      yval <- NULL
      try(yval <- YVAL(), silent=TRUE)
      if(is.null(yval)){
        datasetV <- NULL
      } else {
        datasetV <- data.frame(X = I(as.matrix(dataXVAL())), Y=yval[[1]])
      }
      cv <- input$input_meth_cv_pre
      if(grepl("%",cv)){ # Segments as percent
        seg = round(100/as.numeric(strsplit(cv,"%")[[1]]))
      } else {           # Number of segments
        seg <- as.numeric(cv)
        if(seg == 1){    # Leave-one-out
          seg <- nrow(datasetC)
        }
      }
      incProgress(1/10, detail = "Cross-validating", 100)
      model_pre_selection <<- plsr(Y~X, ncomp = as.numeric(input$input_meth_pre), validation="CV", 
                                   segments=seg, segment.type="consecutive", data=datasetC, x=TRUE)
      ncomp <- max(c(which.min(pls::RMSEP(model_pre_selection)$val[1,1,])-1,1))
      model_pre_selection$best_ncomp <<- ncomp
      last_model_run <<- 1
      df1   <- datasetC[2]
      names(df1) <- "YREF"
      df2   <- data.frame(predict(model_pre_selection, ncomp=ncomp))
      names(df2) <- "predY"
      df3   <- cbind(df1, df2)
      New1 <- write.csv(df3, file = "~/calpred.csv")
      RMSEC  <- pls::RMSEP(model_pre_selection, estimate="train")$val[1,1,ncomp+1]
      RPDc   <- sd(datasetC$Y)/RMSEC
      RMSECV <- pls::RMSEP(model_pre_selection, estimate="CV")$val[1,1,ncomp+1]
      RPDcv  <- sd(datasetC$Y)/RMSECV
      # Validation
      if(!is.null(datasetV)){
        incProgress(1/10, detail = "Predicting")
        df4   <- data.frame(predict(model_pre_selection, datasetV, ncomp=ncomp))
        names(df4) <- "predY1"
        df5   <- datasetV[2]
        names(df5) <- c("YREF")
        df6 <- cbind(df4, df5)
        New2 <- write.csv(df6, file = "~/valpred.csv")
        RMSEP   <- pls::RMSEP(model_pre_selection, newdata=datasetV)$val[1,1,ncomp+1]
        RPDval  <- sd(datasetV$Y)/RMSEP
      }
      df <- data.frame("Variable" = input$text,"Derivative" = input$select_deriv, "Pretreatment" = input$select_pre, "Variable Selection" = input$select_sel,
                       "RMSEC" = RMSEC, "R2Cal" = 100*pls::R2(model_pre_selection, estimate="train")$val[1,1,ncomp+1], "RPDCal" = RPDc,
                       "RMSECV" = RMSECV, "R2CV" = 100*pls::R2(model_pre_selection, estimate="CV")$val[1,1,ncomp+1], "RPDcv" = RPDcv,
                       "RMSEP" = ifelse(is.null(datasetV),NA,RMSEP), "R2Val" = ifelse(is.null(datasetV),NA,100*pls::R2(model_pre_selection, newdata=datasetV)$val[1,1,ncomp+1]), "RPDVal" = ifelse(is.null(datasetV),NA,RPDval),
                       check.names = FALSE)
    })
    df
  })
  
  observeEvent(input$goButton1, {
    newtab <- switch(input$tabs,
                     "ayar" = "widgets",
                     "widgets" = "ayar"
    )
    updateTabItems(session, "tabs", newtab)
  })
  
  ##########POST ANALIYSIS MENU#################################
  
  # Change input field for variable selction based on selection
  observeEvent(input$select_sel,{
    #                    None, VIP, SR,   sMC,  mRMR, iPLS, GA-PLS, IPW-PLS, UVE-PLS
    vals <- as.character(c(0,   1,  5,   0.05,  0.75,  0,    0,       0,       0))
    labs <-           c('Unused', 'cut-off (>0)', 'cut-off (>0)', 'cut-off [0,1]', 'cut-off [0,1]', 'Unused', 'Unused', 'Unused', 'Unused')
    updateTextInput(session, "input_sel", value = vals[as.numeric(input$select_sel)], label = labs[as.numeric(input$select_sel)])
  })
  
  # Change input fields for method based on selection
  observeEvent(input$select_meth,{
    # gamma.best <- 1e-5; cost.best <- 1e+4; epsilon.best <- 0.01
    vals1 <- c("10", "1e-5"); vals2 <- c("10%", "1e+4")
    labs1 <- c("#comp.", "gamma"); labs2 <- c("CV", "cost")
    updateTextInput(session, "input_meth",    value = vals1[as.numeric(input$select_meth)], label = labs1[as.numeric(input$select_meth)])
    updateTextInput(session, "input_meth_cv", value = vals2[as.numeric(input$select_meth)], label = labs2[as.numeric(input$select_meth)])
  })
  
  ####POP-UP PARAMATERS######################################
  observeEvent(req(input$select_sel),
               if(input$select_sel == 6){
                 showModal(modalDialog(
                   title = "iPLS Parameter Setting",
                   "Please input the required parameters!",
                   footer = tagList(
                     fluidRow(
                       column(4,
                              textInput("par_intcomp", label=("Interval comp."), value=10)),
                       column(4,
                              textInput("intervals", label=("# of intervals"), value=8)),
                       column(4,
                              selectInput("met_ipls", ("Selection method"),
                                          choices = list("Forward" = 1, "Backward" = 2), selected = 1))
                       # column(6,
                       #        selectInput("scale", ("Scale"),
                       #                    choices = list("TRUE" = 1, "FALSE" = 2), selected = 2)),
                       # column(6,
                       #        selectInput("center", ("Center"),
                       #                    choices = list("TRUE" = 1, "FALSE" = 2), selected = 1))
                     ),
                     modalButton("OK")
                   )
                 ))}
               else if (input$select_sel == 7){
                 showModal(modalDialog(
                   title = "GA-PLS Parameter Setting",
                   "Please input the required parameters!",
                   footer = tagList(
                     fluidRow(
                       column(4,
                              textInput("par_size", label=("Population size"), value=100)),
                       column(4,
                              textInput("par_thres", label=("Threshold"), value=10)),
                       column(4,
                              textInput("par_iters", label=("Iterations"), value=5))
                       # column(6,
                       #        selectInput("scale", ("Scale"),
                       #                    choices = list("TRUE" = 1, "FALSE" = 2), selected = 2)),
                       # column(6,
                       #        selectInput("center", ("Center"),
                       #                    choices = list("TRUE" = 1, "FALSE" = 2), selected = 1))
                     ),
                     modalButton("OK")
                   )
                 ))
               }
               else if (input$select_sel == 8){
                 showModal(modalDialog(
                   title = "IPW-PLS Parameter Setting",
                   "Please input the required parameters!",
                   footer = tagList(
                     fluidRow(
                       column(4,
                              textInput("par_ipw_iter", label=("Iterations"), value=2)),
                       column(4,
                              textInput("par_ipw_thres", label=("Threshold"), value=0.001)),
                       column(4,
                              selectInput("par_ipw_filter", ("Filter"),
                                          choices = list("RC" = 1, "SR" = 2, "LW" = 3, "VIP" = 4, "sMC" = 5), selected = 2))
                     ),
                     modalButton("OK")
                   )
                 ))
               }
               else if (input$select_sel == 9){
                 showModal(modalDialog(
                   title = "UVE-PLS Parameter Setting",
                   "Please input the required parameters!",
                   footer = tagList(
                     fluidRow(
                       column(4,
                              textInput("par_uve_N", label=("# of MC sims."), value=3)),
                       column(4,
                              textInput("par_uve_ratio", label=("Calibration prop."), value=0.75)),
                       column(4,
                              textInput("par_uve_thres", label=("Noise threshold (NA=auto)"), value="NA"))
                     ),
                     modalButton("OK")
                   )
                 ))
               }
  )
  
  #################
  # Post-analysis
  op1_post <- eventReactive(req(input$btn_analyse), { # input$btn2
    if(!is.null(model_pre_selection)){
      withProgress(message = 'Processing', detail = 'Performing variable selection', value = 0, {
        sel_meth <- as.numeric(input$select_sel)
        sel_par  <- as.numeric(input$input_sel)
        ncomp  <- model_pre_selection$best_ncomp
        X_data <- model_pre_selection$x
        par_intcomp <- as.numeric(input$par_intcomp)
        intervals <- as.numeric(input$intervals)
        par_thres <- as.numeric(input$par_thres)
        par_size <- as.numeric(input$par_size)
        par_iters <- as.numeric(input$par_iters)
        sel_vars <- rep(TRUE, ncol(X_data))
        if(sel_meth > 1){ # Perform variable selection
          if(sel_meth == 2){
            sels <- plsVarSel::VIP(model_pre_selection, ncomp)
            sel_vars <- sels > sel_par
            if(sum(sel_vars) == 0){
              sel_vars[which.max(sels)] <- TRUE
            }
          }
          if(sel_meth == 3){
            sels <- plsVarSel::SR(model_pre_selection, ncomp, X_data)
            sel_vars <- sels > sel_par
            if(sum(sel_vars) == 0){
              sel_vars[which.max(sels)] <- TRUE
            }
          }
          if(sel_meth == 4){
            sels <- plsVarSel::sMC(model_pre_selection, ncomp, X_data, sel_par)
            sel_vars <- sels > attr(sels, "quantile")
            if(sum(sel_vars) == 0){
              sel_vars[which.max(sels)] <- TRUE
            }
          }
          if(sel_meth == 5){
            n_sel    <- round(sel_par*ncol(X_data))
            sel_vars <- plsVarSel::mRMR(model_pre_selection, n_sel, X_data)$selection
          }
          if(sel_meth == 6){
            cv <- input$input_meth_cv_pre
            if(grepl("%",cv)){ # Segments as percent
              seg = round(100/as.numeric(strsplit(cv,"%")[[1]]))
            } else {           # Number of segments
              seg <- as.numeric(cv)
            }
            sel_vars <- mdatools::ipls(dataXCAL(), YCAL(), glob.ncomp = ncomp, cv = seg, int.num = intervals, method = c("forward","backward")[as.numeric(input$met_ipls)])$var.selected
          }
          if(sel_meth == 7){
            sel_vars <- plsVarSel::ga_pls(y = YCAL()[[1]], X = as.matrix(dataXCAL()), GA.threshold = par_thres, iters = par_iters, popSize = par_size)$ga.selection
          }
          if(sel_meth == 8){
            sel_vars <- plsVarSel::ipw_pls(y = YCAL()[[1]], X = as.matrix(dataXCAL()), no.iter = as.numeric(input$par_ipw_iter), IPW.threshold = as.numeric(input$par_ipw_thres), filter = c("RC", "SR", "LW", "VIP", "sMC")[as.numeric(input$par_ipw_filter)])$ipw.selection
          }
          if(sel_meth == 9){
            sel_vars <- plsVarSel::mcuve_pls(y = YCAL()[[1]], X = as.matrix(dataXCAL()), ncomp = ncomp, N = as.numeric(input$par_uve_N), ratio = as.numeric(input$par_uve_ratio), MCUVE.threshold = ifelse(input$par_uve_thres=="NA",NA,as.numeric(input$par_uve_thres)))$mcuve.selection
          }
        }
        selected_variables <<- sel_vars
        
        last_model_run <<- 2
        if(input$select_meth==1){ # PLSR
          incProgress(1/5, detail = "Reading (and pre-processing) data")
          datasetC <- data.frame(X = I(as.matrix(dataXCAL())[,sel_vars,drop=FALSE]), Y=YCAL()[[1]])
          yval <- NULL
          try(yval <- YVAL(), silent=TRUE)
          if(is.null(yval)){
            datasetV <- NULL
          } else {
            datasetV <- data.frame(X = I(as.matrix(dataXVAL())[,sel_vars,drop=FALSE]), Y=YVAL()[[1]])
          }
          print(paste0("Number of selected variables: ", dim(datasetC$X)[2]))
          cv <- input$input_meth_cv
          if(grepl("%",cv)){ # Segments as percent
            seg = round(100/as.numeric(strsplit(cv,"%")[[1]]))
          } else {           # Number of segments
            seg <- as.numeric(cv)
            if(seg == 1){    # Leave-one-out
              seg <- nrow(datasetC)
            }
          }
          incProgress(7/10, detail = "Cross-validating")
          mod <- plsr(Y~X, ncomp = min(c(as.numeric(input$input_meth)),nrow(datasetC)-1, ncol(datasetC$X)), validation="CV", 
                      segments=seg, segment.type="consecutive", data=datasetC)
          ncomp <- max(c(which.min(pls::RMSEP(mod)$val[1,1,])-1,1))
          df1   <- datasetC[2]
          names(df1) <- "YREF"
          df2   <- data.frame(predict(mod, ncomp=ncomp))
          names(df2) <- "predY"
          df3   <- cbind(df1, df2)
          New1 <- write.csv(df3, file = "~/calpred.csv")
          RMSEC  <- pls::RMSEP(mod, estimate="train")$val[1,1,ncomp+1]
          RPDc   <- sd(datasetC$Y)/RMSEC
          RMSECV <- pls::RMSEP(mod, estimate="CV")$val[1,1,ncomp+1]
          RPDcv  <- sd(datasetC$Y)/RMSECV
          if(!is.null(datasetV)){
            incProgress(1/10, detail = "Predicting")
            df4   <- data.frame(predict(mod, datasetV, ncomp=ncomp))
            names(df4) <- "predY1"
            df5   <- datasetV[2]
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New2 <- write.csv(df6, file = "~/valpred.csv")
            RMSEP  <- pls::RMSEP(mod, newdata=datasetV)$val[1,1,ncomp+1]
            RPDval  <- sd(datasetV$Y)/RMSEP
          }
          df <- data.frame("Variable" = input$text, "Derivative" = input$select_deriv, "Pretreatment" = input$select_pre,"Variable Selection" = input$select_sel, 
                           "RMSEC" = RMSEC, "R2Cal" = 100*pls::R2(mod, estimate="train")$val[1,1,ncomp+1], "RPDCal" = RPDc, 
                           "RMSECV" = RMSECV, "R2CV" = 100*pls::R2(mod, estimate="CV")$val[1,1,ncomp+1], "RPDcv" = RPDcv, 
                           "RMSEP" = ifelse(is.null(datasetV),NA,RMSEP), "R2Val" = ifelse(is.null(datasetV),NA,100*pls::R2(mod, newdata=datasetV)$val[1,1,ncomp+1]), "RPDVal" = ifelse(is.null(datasetV),NA,RPDval), 
                           check.names = FALSE)
          # df
        }
        if (input$select_meth==2) {
          #Fit a model. The function syntax is very similar to lm function
          epsilon.best <- 0.01 # gamma.best <- 1e-5; cost.best <- 1e+4; 
          incProgress(1/5, detail = "Reading (and pre-processing) data")
          model_svm <- svm(x=as.matrix(dataXCAL())[,sel_vars,drop=FALSE] , y=YCAL() , type = "eps-regression", kernel = "radial",
                           cost = as.numeric(input$input_meth_cv), gamma = as.numeric(input$input_meth), epsilon = epsilon.best)
          
          # Use the predictions on the data
          incProgress(7/10, detail = "Modelling")
          predY = predict(model_svm, as.matrix(dataXCAL())[,sel_vars,drop=FALSE])
          df1 <- data.frame(YCAL())
          names(df1) <- c("YREF")
          df2 <- data.frame(predY)
          df3 <- cbind(df1, df2)
          New1 <- write.csv(df3, file = "~/calpred.csv")
          mae = MAE(df3$YREF, df3$predY)
          rmse = RMSE(df3$YREF, df3$predY)
          r2 = caret::R2(df3$predY, df3$YREF,  form = "traditional")
          RPD = RPD(df3$predY, df3$YREF)
          yval <- NULL
          try(yval <- YVAL(), silent=TRUE)
          if(is.null(yval)){
            predY1 <- NULL
          } else {
            incProgress(1/10, detail = "Predicting")
            predY1 = predict(model_svm, as.matrix(dataXVAL())[,sel_vars,drop=FALSE])
            df4 <- data.frame(predY1)
            df5 <- data.frame(YVAL())
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New2 <- write.csv(df6, file = "~/valpred.csv")
            mae1 = MAE(df6$YREF, df6$predY1)
            rmse1 = RMSE(df6$YREF, df6$predY1)
            r21 = caret::R2(df6$predY1, df6$YREF,  form = "traditional")
            RPD1 = RPD(df6$predY1, df6$YREF)
          }
          
          df <- data.frame("Variable" = input$text, "Derivative" = input$select_deriv, "Pretreatment" = input$select_pre, "Variable Selection" = input$select_sel, 
                           "RMSEC" = rmse, "R2Cal" = 100*r2, "RPDCal" = RPD, 
                           "RMSEP" = ifelse(is.null(predY1),NA,rmse1), "R2Val" = ifelse(is.null(predY1),NA,100*r21), "RPDVal" = ifelse(is.null(predY1),NA,RPD1), 
                           check.names = FALSE)
        }
        df
      })
    }
  })
  
  # Fill result table (pre-analysis)
  output$Results1 <- DT::renderDataTable({
    dt <- datatable(
      op1(), extensions = 'Buttons', options = list(autoWidth = FALSE, scrollX = TRUE, columnDefs = list(list(width = '10px', targets = c(1, 13))),
                                                    dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel')
      )
    ) %>%
      formatRound(c('RMSEC', 'R2Cal', 'RPDCal', 'RMSECV', 'R2CV', 'RPDcv', 'RMSEP', 'R2Val', 'RPDVal'), 2)  
  })
  output$Results2 <- DT::renderDataTable({
    req(input$btn_analyse)
    if (input$select_meth==1) {
      op21 <- read.csv("~/plstum.csv")
      dt <- datatable(
        op21[2:14], extensions = 'Buttons', options = list(autoWidth = FALSE, scrollX = TRUE, columnDefs = list(list(width = '10px', targets = c(1, 13))),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel')
        )
      ) %>%
        formatRound(c('RMSEC', 'R2Cal', 'RPDCal', 'RMSECV', 'R2CV', 'RPDcv', 'RMSEP', 'R2Val', 'RPDVal'), 2)
    } else if (input$select_meth==2) {
      op22 <- read.csv("~/svmtum.csv")
      dt <- datatable(
        op22[2:11], extensions = 'Buttons', options = list(autoWidth = FALSE, scrollX = TRUE, columnDefs = list(list(width = '10px', targets = c(1, 10))),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel')
        )
      ) %>%
        formatRound(c('RMSEC', 'R2Cal', 'RPDCal', 'RMSEP', 'R2Val', 'RPDVal'), 2)
    }
    
  })
  
  # Fill result table (post-analysis)
  observeEvent(input$btn_analyse, { # input$btn2
    req(input$select_meth==1)
    df <- data.frame(op1_post())
    New1 <- write.csv(df, file = "~/plsson.csv")
    samtable1 <- read.csv("~/plstum.csv")
    data <- rbind(samtable1[,-1], df)
    New2 <- write.csv(data, file = "~/plstum.csv")
  })
  
  observeEvent(input$btn_analyse, { # input$btn2
    req(input$select_meth==2) #4 | input$select2==5 | input$select2==6)
    df <- data.frame(op1_post())
    New1 <- write.csv(df, file = "~/svmson.csv")
    samtable1 <- read.csv("~/svmtum.csv")
    data <- rbind(samtable1[,-1], df)
    New2 <- write.csv(data, file = "~/svmtum.csv")
  })
  
  # Plot raw data
  op2 <- eventReactive(input$btn_spec_plot, {#input$btn1
    mdaplot(dataXCAL(), type = 'l')
  })    
  output$plot1 <- renderPlot(
    op2()
  )
  
  # Variable selection plot
  op2_post <- eventReactive(input$btn_analyse, {#input$btn1
    X <- dataXCAL()
    xnam <- colnames(X)
    if(length(xnam) == ncol(X) && length(grep("^[-0-9.]+[^0-9]*$", 
                                              xnam)) == length(xnam)) {
      labels <- sub("[^0-9]*$", "", 
                    xnam)
      xnum <- as.numeric(labels)
    } else {
      xnum <- 1:ncol(X)
    }
    if(is.logical(selected_variables[1])){
      sel_vars <- which(selected_variables)
    } else {
      sel_vars <- selected_variables
    }
    matplot(xnum,t(X), type='l', lty=1, col=rgb(50/255,70/255,200/255), xlab="variables", ylab="intensity")
    points(xnum[sel_vars], colMeans(X[,sel_vars, drop=FALSE]), col='red', pch=20)
    points(xnum[sel_vars], rep(min(X),length(sel_vars)), col='red', pch=20)
  })
  output$plot_sel <- renderPlot(
    op2_post()
  )
  
  # observeEvent(input$btn_analyse_pre, { #input$btn2
  #   cat("Showing", input$file1, "rows\n")
  # })
  # observeEvent(input$btn_analyse, { #input$btn2
  #   cat("Showing", input$file1, "rows\n")
  # })
  
  # Predicted vs reference plot (pre-analysis)
  op3 <- eventReactive(req(input$btn_plot_pre), {#btn3 ...req(input$btn_plot_pre), 
    df3 <- read.csv("~/calpred.csv")
    yval <- NULL
    try(yval <- YVAL(), silent=TRUE)
    if(!is.null(yval)){
      df6 <- read.csv("~/valpred.csv")
      p1 <- gridExtra::grid.arrange(ggplot(df3, aes(x = YREF, y = predY)) + geom_point(color = "black", pch=1, size=5) + geom_smooth(formula = y~x, method = "lm", se = TRUE) +
                                      theme_bw() + theme(                              
                                        axis.title.x = element_text(face="bold", color="black", size=16),
                                        axis.title.y = element_text(face="bold", color="black", size=16),
                                        plot.title = element_text(face="bold", color = "black", size=16),
                                        legend.position=c(1,1),
                                        legend.justification=c(1,1)) +
                                      labs(x = "Reference Analysis", 
                                           y = "Model Prediction", 
                                           title= "Calibration Plot"),
                                    ggplot(df6, aes(x = YREF, y = predY1)) + geom_point(color = "black", pch=1, size=5) + geom_smooth(formula = y~x, method = "lm", se = TRUE) +
                                      theme_bw() + theme(                              
                                        axis.title.x = element_text(face="bold", color="black", size=16),
                                        axis.title.y = element_text(face="bold", color="black", size=16),
                                        plot.title = element_text(face="bold", color = "black", size=16),
                                        legend.position=c(1,1),
                                        legend.justification=c(1,1)) +
                                      labs(x = "Reference Analysis", 
                                           y = "Model Prediction", 
                                           title= "External Validation Plot"), nrow = 1
      )
    } else {
      p1 <- gridExtra::grid.arrange(ggplot(df3, aes(x = YREF, y = predY)) + geom_point(color = "black", pch=1, size=5) + geom_smooth(formula = y~x, method = "lm", se = TRUE) +
                                      theme_bw() + theme(                              
                                        axis.title.x = element_text(face="bold", color="black", size=16),
                                        axis.title.y = element_text(face="bold", color="black", size=16),
                                        plot.title = element_text(face="bold", color = "black", size=16),
                                        legend.position=c(1,1),
                                        legend.justification=c(1,1)) +
                                      labs(x = "Reference Analysis", 
                                           y = "Model Prediction", 
                                           title= "Calibration Plot"), nrow = 1
      )
    }
    p1
  })    
  
  # Predicted vs reference plot (post-analysis)
  op3_post <- eventReactive(req(input$btn_plot), {#btn3
    df3 <- read.csv("~/calpred.csv")
    yval <- NULL
    try(yval <- YVAL(), silent=TRUE)
    if(!is.null(yval)){
      df6 <- read.csv("~/valpred.csv")
      p1 <- gridExtra::grid.arrange(ggplot(df3, aes(x = YREF, y = predY)) + geom_point(color = "black", pch=1, size=5) + geom_smooth(formula = y~x, method = "lm", se = TRUE) +
                                      theme_bw() + theme(                              
                                        axis.title.x = element_text(face="bold", color="black", size=16),
                                        axis.title.y = element_text(face="bold", color="black", size=16),
                                        plot.title = element_text(face="bold", color = "black", size=16),
                                        legend.position=c(1,1),
                                        legend.justification=c(1,1)) +
                                      labs(x = "Reference Analysis", 
                                           y = "Model Prediction", 
                                           title= "Calibration Plot"),
                                    ggplot(df6, aes(x = YREF, y = predY1)) + geom_point(color = "black", pch=1, size=5) + geom_smooth(formula = y~x, method = "lm", se = TRUE) +
                                      theme_bw() + theme(                              
                                        axis.title.x = element_text(face="bold", color="black", size=16),
                                        axis.title.y = element_text(face="bold", color="black", size=16),
                                        plot.title = element_text(face="bold", color = "black", size=16),
                                        legend.position=c(1,1),
                                        legend.justification=c(1,1)) +
                                      labs(x = "Reference Analysis", 
                                           y = "Model Prediction", 
                                           title= "External Validation Plot"), nrow = 1
      )
    } else {
      p1 <- gridExtra::grid.arrange(ggplot(df3, aes(x = YREF, y = predY)) + geom_point(color = "black", pch=1, size=5) + geom_smooth(formula = y~x, method = "lm", se = TRUE) +
                                      theme_bw() + theme(                              
                                        axis.title.x = element_text(face="bold", color="black", size=16),
                                        axis.title.y = element_text(face="bold", color="black", size=16),
                                        plot.title = element_text(face="bold", color = "black", size=16),
                                        legend.position=c(1,1),
                                        legend.justification=c(1,1)) +
                                      labs(x = "Reference Analysis", 
                                           y = "Model Prediction", 
                                           title= "Calibration Plot"), nrow = 1)
    }
    p1
  })    
  
  output$plot2 <- renderPlot(
    op3()
  )
  output$plot2post <- renderPlot(
    op3_post()
  )
}

shinyApp(ui, server)