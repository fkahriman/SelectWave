#
library(shiny)
library(DT)
library(mdatools)
library(plsVarSel)
library(e1071)
library(prospectr)
library(pls)
library(caret)
library(chillR)
library(ggplot2)
library(magrittr)

## Veri Giris Modulu----------------------------------------------------------------
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
        msg <- sprintf("Dosya %s yuklendi", userFile()$name)
        cat(msg, "\n")
    })
    
    # Return the reactive that yields the data frame
    return(dataframe)
}

## Veri Giris Modulu----------------------------------------------------------------

# BAFR App
ui <- fluidPage(    # Application title
    titlePanel(img(src = "logo.png", height = 51, width = 151,
                   tags$head(
                       tags$style(HTML("
                                   .navbar .navbar-nav {float: right}
                                   .navbar .navbar-header {float: left}
                                   "))
                   ))),
    navbarPage("SelectWave",
               tabPanel("ABOUT",
                        sidebarLayout(
                            sidebarPanel(
                                br(),
                                br(),
                                br(),
                                br(),
                                img(src = "Rstudio.png", height = 75, width = 75),
                                img(src = "Shiny.png", height = 90, width = 75),
                                br(),
                                "SelectWave application is developed using",
                                span("RStudio", style = "color:blue"),"and",span("Shiny", style = "color:blue"),
                            ),
                            mainPanel(
                                h1("About SelectWave"),
                                p("SelectWave application is developed using shiny package under RStudio platform."),
                                br(),
                                p("For downloading portable version of app and for more detailed information, please visit to",
                                  tags$a("BAF Elektronik Yazilim Tarim Inc.",
                                    href = "http://www.baf-eyt.com.tr")),
                                br(),
                                p("Referans=>SelectWave:Wavelenght Selection and Multivarite Modelling."),
                                h2("Specialities"),
                                p("- It is an open-source app."),
                                p("- It is no need to code writing."),
                                p("- It has well-known multivariate modelling techniques."),
                                p("- GNU licenced."),
                                p("- Developed for scientific studies.")
                            )
                        )
               ),
               ## SPEKTRAL ANALIZLER----------------------------------------------------------------
               tabPanel("SPECTRAL ANALYSES",
                        sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                csvFileInput("file1", "XCAL (.txt format)"),
                                csvFileInput("file2", "YCAL (.txt format)"),
                                csvFileInput("file3", "XVAL (.txt format)"),
                                csvFileInput("file4", "YVAL (.txt format)"),
                                br(),
                                textInput("text", "VARIABLE NAME:"),
                                br(),
                                radioButtons("radio1", h4("Select Pretreatment"),
                                             choices = list("No Pretreatment" = 1, "FD" = 2,
                                                            "SD" = 3, "SNV" = 4, "MSC" = 5, "FD+SNV" = 6, "FD+MSC" = 7, "SD+SNV" = 8, "SD+MSC" =9), selected = 1),
                                selectInput("select2", h4("Select Analysis Method"),
                                            choices = list("PLS" = 1, "VIP-PLS" = 2, "SR-PLS" = 3, "SVM" = 4, "VIP-SVM" = 5, "SR-SVM" = 6), selected = 1),
                                h4("ONLY FIRST TIME USERS"),
                                actionButton("btn4", "CREATE MANDATORY FILES"),
                                br(),
                                h4("ANALYSIS"),
                                actionButton("btn1", "SHOW SPECTRAL PLOT"),
                                br(),
                                br(),
                                actionButton("btn2", "ANALYZE"),
                                br(),
                                br(),
                                actionButton("btn3", "SHOW PREDICTION PLOT")
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Data Input", tableOutput(outputId = "table1")),
                                    tabPanel("Spectral Plot", plotOutput(outputId = "plot1")),
                                    tabPanel("Analysis Result", DT::dataTableOutput(outputId = "Results1")),
                                    tabPanel("Model Plots", plotOutput(outputId = "plot2"))
                                )
                            )
                        ))
    ))

# Define server logic to read selected file ----
server <- function(input, output,session) {
    
    XCAL <- callModule(csvFile, "file1",
                       stringsAsFactors = FALSE)
    YCAL <- callModule(csvFile, "file2",
                       stringsAsFactors = FALSE)
    XVAL <- callModule(csvFile, "file3",
                       stringsAsFactors = FALSE)
    YVAL <- callModule(csvFile, "file4",
                       stringsAsFactors = FALSE)
    
    dataXCAL <- reactive(
        
        if (input$radio1==1) {  
            dataXCAL=XCAL()
        } else if (input$radio1==2) {
            dataXCAL= gapDer(X = XCAL(), m = 1, w = 11, s = 1)
        }
        else if (input$radio1==3) {  
            dataXCAL=gapDer(X = XCAL(), m = 2, w = 11, s = 1)
        }
        else if (input$radio1==4) {  
            dataXCAL=prep.snv(XCAL())
        }
        else if (input$radio1==5) {  
            dataXCAL=prep.msc(t(t(XCAL())))
        }
        else if (input$radio1==6) {
            dataXCAL1 <- gapDer(X = XCAL(), m = 1, w = 11, s = 3)
            dataXCAL=prep.snv(dataXCAL1)
        }
        else if (input$radio1==7) {  
            dataXCAL1 <- gapDer(X = XCAL(), m = 1, w = 11, s = 3)
            dataXCAL=prep.msc(dataXCAL1)
        }
        else if (input$radio1==8) {  
            dataXCAL1 <- gapDer(X = XCAL(), m = 2, w = 11, s = 3)
            dataXCAL=prep.snv(dataXCAL1)
        }
        else if (input$radio1==9) {  
            dataXCAL1 <- gapDer(X = XCAL(), m = 2, w = 11, s = 3)
            dataXCAL=prep.msc(dataXCAL1)
        }
    )
    
    dataXVAL <- reactive(
        
        if (input$radio1==1) {  
            dataXCAL=XVAL()
        } else if (input$radio1==2) {
            dataXVAL= gapDer(X = XVAL(), m = 1, w = 11, s = 1)
        }
        else if (input$radio1==3) {  
            dataXVAL=gapDer(X = XVAL(), m = 2, w = 11, s = 1)
        }
        else if (input$radio1==4) {  
            dataXVAL=prep.snv(XVAL())
        }
        else if (input$radio1==5) {  
            dataXVAL=prep.msc(t(t(XVAL())))
        }
        else if (input$radio1==6) {
            dataXVAL1 <- gapDer(X = XVAL(), m = 1, w = 11, s = 3)
            dataXVAL=prep.snv(dataXVAL1)
        }
        else if (input$radio1==7) {  
            dataXVAL1 <- gapDer(X = XVAL(), m = 1, w = 11, s = 3)
            dataXVAL=prep.msc(dataXVAL1)
        }
        else if (input$radio1==8) {  
            dataXVAL1 <- gapDer(X = XVAL(), m = 2, w = 11, s = 3)
            dataXVAL=prep.snv(dataXVAL1)
        }
        else if (input$radio1==9) {  
            dataXVAL1 <- gapDer(X = XVAL(), m = 2, w = 11, s = 3)
            dataXVAL=prep.msc(dataXVAL1)
        }
    )
    
    output$table1 <- renderTable({
        head(dataXCAL())
    })
    
    observeEvent(input$btn2, {
        cat("Showing", input$file1, "rows\n")
    })
    

    showModal(modalDialog(
        title = "Important message",
        "If you are a first time user, please create mandatory files via click create mandatory files button under spectral analysis tab!",
        footer = tagList(
          modalButton("OK")
        )
      ))

    observeEvent(input$btn4, {
        # Create required files for firt time users
      df <- data.frame("Variable" = "","Pretreatment" = "", "Regression Method" = "", "RMSEC" = "", "R2Cal" = "", "RPDCal" = "", "RMSECV" = "", "R2Val" = "", "RPDVal" = "", check.names = FALSE)
        New1 <- write.csv(df, file = "~/plstum.csv")
        New2 <- write.csv(df, file = "~/svmtum.csv")
    })

    op1 <- eventReactive(req(input$btn2), {
        if (input$select2==1) {  
            Name <- input$text
            m <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', info = "YREF prediction model")
            ncomp <- m$ncomp.selected
            res = predict(m, dataXVAL(), YVAL())
            df1 <- data.frame(YCAL())
            names(df1) <- c("YREF")
            df2 <- as.data.frame(m$calres$y.pred)[ncomp]
            names(df2) <- c("predYsvm")
            df3 <- cbind(df1, df2)
            df4 <- as.data.frame(res$y.pred)[ncomp]
            names(df4) <- c("predYsvm1")
            df5 <- data.frame(YVAL())
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New1 <- write.csv(df3, file = "~/calpred.csv")
            New2 <- write.csv(df6, file = "~/valpred.csv")
            df <- data.frame("Variable" = input$text,"Pretreatment" = input$radio1, "Regression Method"= input$select2, "RMSEC" = m$calres$rmse[ncomp], "R2Cal" = m$calres$r2[ncomp], "RPDCal" = m$calres$rpd[ncomp], "RMSECV" = m$cvres$rmse[ncomp], "R2Val" = m$cvres$r2[ncomp], "RPDVal" = m$cvres$rpd[ncomp], check.names = FALSE)
            df
        }
        else if (input$select2==2) {  
            Name <- input$text
            m <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', info = "YREF prediction model")
            m1 <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', exclcols = getVIPScores(m, ncomp = ncomp.selected) < 0.5)
            ncomp <- m1$ncomp.selected
            res = predict(m1, dataXVAL(), YVAL())
            df1 <- data.frame(YCAL())
            names(df1) <- c("YREF")
            df2 <- as.data.frame(m1$calres$y.pred)[ncomp]
            names(df2) <- c("predYsvm")
            df3 <- cbind(df1, df2)
            df4 <- as.data.frame(res$y.pred)[ncomp]
            names(df4) <- c("predYsvm1")
            df5 <- data.frame(YVAL())
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New1 <- write.csv(df3, file = "~/calpred.csv")
            New2 <- write.csv(df6, file = "~/valpred.csv")
            df <- data.frame("Variable" = input$text,"Pretreatment" = input$radio1, "Regression Method" = input$select2, "RMSEC" = m1$calres$rmse[ncomp], "R2Cal" = m1$calres$r2[ncomp], "RPDCal" = m1$calres$rpd[ncomp], "RMSECV" = m1$cvres$rmse[ncomp], "R2Val" = m1$cvres$r2[ncomp], "RPDVal" = m1$cvres$rpd[ncomp], check.names = FALSE)
            df
        }
        else if (input$select2==3) {  
            Name <- input$text
            m <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', info = "YREF prediction model")
            m1 <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, exclcols = getSelectivityRatio(m, ncomp = NULL) < 0.0003)
            ncomp <- m1$ncomp.selected
            res = predict(m1, dataXVAL(), YVAL())
            df1 <- data.frame(YCAL())
            names(df1) <- c("YREF")
            df2 <- as.data.frame(m1$calres$y.pred)[ncomp]
            names(df2) <- c("predYsvm")
            df3 <- cbind(df1, df2)
            df4 <- as.data.frame(res$y.pred)[ncomp]
            names(df4) <- c("predYsvm1")
            df5 <- data.frame(YVAL())
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New1 <- write.csv(df3, file = "~/calpred.csv")
            New2 <- write.csv(df6, file = "~/valpred.csv")
            df <- data.frame("Variable" = input$text,"Pretreatment" = input$radio1, "Regression Method" = input$select2, "RMSEC" = m1$calres$rmse[ncomp], "R2Cal" = m1$calres$r2[ncomp], "RPDCal" = m1$calres$rpd[ncomp], "RMSECV" = m1$cvres$rmse[ncomp], "R2Val" = m1$cvres$r2[ncomp], "RPDVal" = m1$cvres$rpd[ncomp], check.names = FALSE)
            df
        }
        else if (input$select2==4) {  
            #Fit a model. The function syntax is very similar to lm function
            gamma.best <- 1e-5; cost.best <- 1e+4; epsilon.best <- 0.01
            model_svm <- svm(x=dataXCAL() , y=YCAL() , type = "eps-regression", kernel = "linear",
                             cost = cost.best, gamma = gamma.best, epsilon = epsilon.best)
            #Predict using SVM regression
            predYsvm = predict(model_svm, dataXCAL())
            predYsvm1 = predict(model_svm, dataXVAL())
            df1 <- data.frame(YCAL())
            names(df1) <- c("YREF")
            df2 <- data.frame(predYsvm)
            df3 <- cbind(df1, df2)
            df4 <- data.frame(predYsvm1)
            df5 <- data.frame(YVAL())
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New1 <- write.csv(df3, file = "~/calpred.csv")
            New2 <- write.csv(df6, file = "~/valpred.csv")
            mae = MAE(df3$YREF, df3$predYsvm)
            rmse = RMSE(df3$YREF, df3$predYsvm)
            r2 = R2(YCAL(), predYsvm, form = "traditional")
            RPD = RPD(YCAL(), predYsvm)
            mae1 = MAE(df6$YREF, df6$predYsvm1)
            rmse1 = RMSE(df6$YREF, df6$predYsvm1)
            r21 = R2(YVAL(), predYsvm1, form = "traditional")
            RPD1 = RPD(YVAL(), predYsvm1)
            
            df <- data.frame("Variable" = input$text,"Pretreatment" = input$radio1, "Regression Method" = input$select2, "RMSEC" = rmse, "R2Cal" = r2, "RPDCal" = RPD, "RMSECV" = rmse1, "R2Val" = r21, "RPDVal" = RPD1, check.names = FALSE)
            df
        }
        
        else if (input$select2==5) {  
            m <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', info = "YREF prediction model")
            m1 <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', exclcols = getVIPScores(m, ncomp = ncomp.selected) < 0.5)
            ncomp <- m1$ncomp.selected
            exclcols = (getVIPScores(m1, ncomp = ncomp.selected)<0.5)
            remove <-  c("TRUE") 
            subset <- exclcols[-which(exclcols %in% remove),]
            subset1 <- as.data.frame(subset)
            #Fit a model. The function syntax is very similar to lm function
            gamma.best <- 1e-5; cost.best <- 1e+4; epsilon.best <- 0.01
            model_svm <- svm(x=dataXCAL()[,row.names(subset1)], y=YCAL() , type = "eps-regression", kernel = "radial",
                             cost = cost.best, gamma = gamma.best, epsilon = epsilon.best)
            
            #Use the predictions on the data
            predYsvm = predict(model_svm, dataXCAL()[,row.names(subset1)])
            predYsvm1 = predict(model_svm, dataXVAL()[,row.names(subset1)])
            df1 <- data.frame(YCAL())
            names(df1) <- c("YREF")
            df2 <- data.frame(predYsvm)
            df3 <- cbind(df1, df2)
            df4 <- data.frame(predYsvm1)
            df5 <- data.frame(YVAL())
            names(df5) <- c("YREF")
            df6 <- cbind(df4, df5)
            New1 <- write.csv(df3, file = "~/calpred.csv")
            New2 <- write.csv(df6, file = "~/valpred.csv")
            mae = MAE(df3$YREF, df3$predYsvm)
            rmse = RMSE(df3$YREF, df3$predYsvm)
            r2 = R2(df3$predYsvm, df3$YREF,  form = "traditional")
            RPD = RPD(df3$predYsvm, df3$YREF)
            mae1 = MAE(df6$YREF, df6$predYsvm1)
            rmse1 = RMSE(df6$YREF, df6$predYsvm1)
            r21 = R2(df6$predYsvm1, df6$YREF,  form = "traditional")
            RPD1 = RPD(df6$predYsvm1, df6$YREF)
            
            df <- data.frame("Variable" = input$text,"Pretreatment" = input$radio1, "Regression Method" = input$select2, "RMSEC" = rmse, "R2Cal" = r2, "RPDCal" = RPD, "RMSECV" = rmse1, "R2Val" = r21, "RPDVal" = RPD1, check.names = FALSE)
            df
        }
        else if (input$select2==6) {  
          m <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', info = "YREF prediction model")
          m1 <- pls(dataXCAL(), YCAL(), scale = T, cv = 1, ncomp.selcrit = 'min', exclcols = getSelectivityRatio(m, ncomp = NULL) < 0.0003)
          ncomp <- m1$ncomp.selected
          exclcols = (getSelectivityRatio(m1, ncomp = ncomp.selected)<1)
          remove <-  c("TRUE") 
          subset <- exclcols[-which(exclcols %in% remove),]
          subset1 <- as.data.frame(subset)
          #Fit a model. The function syntax is very similar to lm function
          gamma.best <- 1e-5; cost.best <- 1e+4; epsilon.best <- 0.01
          model_svm <- svm(x=dataXCAL()[,row.names(subset1)], y=YCAL() , type = "eps-regression", kernel = "radial",
                           cost = cost.best, gamma = gamma.best, epsilon = epsilon.best)
          
          #Use the predictions on the data
          predYsvm = predict(model_svm, dataXCAL()[,row.names(subset1)])
          predYsvm1 = predict(model_svm, dataXVAL()[,row.names(subset1)])
          df1 <- data.frame(YCAL())
          names(df1) <- c("YREF")
          df2 <- data.frame(predYsvm)
          df3 <- cbind(df1, df2)
          df4 <- data.frame(predYsvm1)
          df5 <- data.frame(YVAL())
          names(df5) <- c("YREF")
          df6 <- cbind(df4, df5)
          New1 <- write.csv(df3, file = "~/calpred.csv")
          New2 <- write.csv(df6, file = "~/valpred.csv")
          mae = MAE(df3$YREF, df3$predYsvm)
          rmse = RMSE(df3$YREF, df3$predYsvm)
          r2 = R2(df3$predYsvm, df3$YREF,  form = "traditional")
          RPD = RPD(df3$predYsvm, df3$YREF)
          mae1 = MAE(df6$YREF, df6$predYsvm1)
          rmse1 = RMSE(df6$YREF, df6$predYsvm1)
          r21 = R2(df6$predYsvm1, df6$YREF,  form = "traditional")
          RPD1 = RPD(df6$predYsvm1, df6$YREF)
          
          df <- data.frame("Variable" = input$text,"Pretreatment" = input$radio1, "Regression Method" = input$select2, "RMSEC" = rmse, "R2Cal" = r2, "RPDCal" = RPD, "RMSECV" = rmse1, "R2Val" = r21, "RPDVal" = RPD1, check.names = FALSE)
          df
        }
    })
    
    output$Results1 <- DT::renderDataTable({
      datatable(
        op1(), extensions = 'Buttons', options = list(autoWidth = TRUE, columnDefs = list(list(width = '50px', targets = c(1, 9))),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        )
      ) %>%
        formatRound(c('RMSEC', 'R2Cal', 'RPDCal', 'RMSECV', 'R2Val', 'RPDVal'), 2)  
  })
    
    observeEvent(input$btn2, {
        req(input$select2==1 | input$select2==2 | input$select2==3)
        df <- data.frame(op1())
        New1 <- write.csv(df, file = "~/plsson.csv")
        #dir <- "~/"
        #New1 <- write.csv(df, {file = file.path(dir, paste("plsson", '.csv', sep=''))})
        samtable1 <- read.csv("~/plstum.csv")
        #files <- list.files(file.path(dir), full.names = TRUE)
        #data <- lapply(files, read.csv, stringsAsFactors = FALSE)
        data <- rbind(samtable1[,-1], df)
        New2 <- write.csv(data, file = "~/plstum.csv")
        #New2 <- write.csv(data, {file = file.path(dir, paste("plstum", '.csv', sep=''))})
        #samtable2 <- t(samtable1[,-1])
    })
    
    observeEvent(input$btn2, {
        req(input$select2==4 | input$select2==5 | input$select2==6)
        df <- data.frame(op1())
        New1 <- write.csv(df, file = "~/svmson.csv")
        #dir <- "~/"
        #New1 <- write.csv(df, {file = file.path(dir, paste("plsson", '.csv', sep=''))})
        samtable1 <- read.csv("~/svmtum.csv")
        #files <- list.files(file.path(dir), full.names = TRUE)
        #data <- lapply(files, read.csv, stringsAsFactors = FALSE)
        data <- rbind(samtable1[,-1], df)
        New2 <- write.csv(data, file = "~/svmtum.csv")
        #New2 <- write.csv(data, {file = file.path(dir, paste("plstum", '.csv', sep=''))})
        #samtable2 <- t(samtable1[,-1])
    })
    
    
    op2 <- eventReactive(input$btn1, {
        mdaplot(dataXCAL(), type = 'l')
    })    
    
    output$plot1 <- renderPlot(
        op2()
    )
    
    observeEvent(input$btn2, {
        cat("Showing", input$file1, "rows\n")
    })
    
    op3 <- eventReactive(req(input$btn3), {
        if (input$select2==1) {
          df3 <- read.csv("~/calpred.csv")
          df6 <- read.csv("~/valpred.csv")
          p1 <- gridExtra::grid.arrange(ggplot(df3) + geom_point(aes(x = df3$YREF, y = df3$predYsvm), color = "black", pch=1, size=5) + geom_smooth(aes(x = df3$YREF, y = df3$predYsvm), method = "lm", se = TRUE) +
                                          theme_bw() + theme(                              
                                            axis.title.x = element_text(face="bold", color="black", size=16),
                                            axis.title.y = element_text(face="bold", color="black", size=16),
                                            plot.title = element_text(face="bold", color = "black", size=16),
                                            legend.position=c(1,1),
                                            legend.justification=c(1,1)) +
                                          labs(x="Reference Analysis", 
                                               y = "Model Prediction", 
                                               title= "Calibration Plot"),
                                        ggplot(df6) + geom_point(aes(x = df6$YREF, y = df6$predYsvm1), color = "black", pch=1, size=5) + geom_smooth(aes(x = df6$YREF, y = df6$predYsvm1), method = "lm", se = TRUE) +
                                          theme_bw() + theme(                              
                                            axis.title.x = element_text(face="bold", color="black", size=16),
                                            axis.title.y = element_text(face="bold", color="black", size=16),
                                            plot.title = element_text(face="bold", color = "black", size=16),
                                            legend.position=c(1,1),
                                            legend.justification=c(1,1)) +
                                          labs(x="Reference Analysis", 
                                               y = "Model Prediction", 
                                               title= "External Validation Plot"), nrow = 1
          )
          p1}
        
        else if (input$select2==2)
        {
          df3 <- read.csv("~/calpred.csv")
          df6 <- read.csv("~/valpred.csv")
          p1 <- gridExtra::grid.arrange(ggplot(df3) + geom_point(aes(x = df3$YREF, y = df3$predYsvm), color = "black", pch=1, size=5) + geom_smooth(aes(x = df3$YREF, y = df3$predYsvm), method = "lm", se = TRUE) +
                                          theme_bw() + theme(                              
                                            axis.title.x = element_text(face="bold", color="black", size=16),
                                            axis.title.y = element_text(face="bold", color="black", size=16),
                                            plot.title = element_text(face="bold", color = "black", size=16),
                                            legend.position=c(1,1),
                                            legend.justification=c(1,1)) +
                                          labs(x="Reference Analysis", 
                                               y = "Model Prediction", 
                                               title= "Calibration Plot"),
                                        ggplot(df6) + geom_point(aes(x = df6$YREF, y = df6$predYsvm1), color = "black", pch=1, size=5) + geom_smooth(aes(x = df6$YREF, y = df6$predYsvm1), method = "lm", se = TRUE) +
                                          theme_bw() + theme(                              
                                            axis.title.x = element_text(face="bold", color="black", size=16),
                                            axis.title.y = element_text(face="bold", color="black", size=16),
                                            plot.title = element_text(face="bold", color = "black", size=16),
                                            legend.position=c(1,1),
                                            legend.justification=c(1,1)) +
                                          labs(x="Reference Analysis", 
                                               y = "Model Prediction", 
                                               title= "External Validation Plot"), nrow = 1
          )
          p1}
        
        else if (input$select2==3)
        {
          df3 <- read.csv("~/calpred.csv")
          df6 <- read.csv("~/valpred.csv")
          p1 <- gridExtra::grid.arrange(ggplot(df3) + geom_point(aes(x = df3$YREF, y = df3$predYsvm), color = "black", pch=1, size=5) + geom_smooth(aes(x = df3$YREF, y = df3$predYsvm), method = "lm", se = TRUE) +
                                          theme_bw() + theme(                              
                                            axis.title.x = element_text(face="bold", color="black", size=16),
                                            axis.title.y = element_text(face="bold", color="black", size=16),
                                            plot.title = element_text(face="bold", color = "black", size=16),
                                            legend.position=c(1,1),
                                            legend.justification=c(1,1)) +
                                          labs(x="Reference Analysis", 
                                               y = "Model Prediction", 
                                               title= "Calibration Plot"),
                                        ggplot(df6) + geom_point(aes(x = df6$YREF, y = df6$predYsvm1), color = "black", pch=1, size=5) + geom_smooth(aes(x = df6$YREF, y = df6$predYsvm1), method = "lm", se = TRUE) +
                                          theme_bw() + theme(                              
                                            axis.title.x = element_text(face="bold", color="black", size=16),
                                            axis.title.y = element_text(face="bold", color="black", size=16),
                                            plot.title = element_text(face="bold", color = "black", size=16),
                                            legend.position=c(1,1),
                                            legend.justification=c(1,1)) +
                                          labs(x="Reference Analysis", 
                                               y = "Model Prediction", 
                                               title= "External Validation Plot"), nrow = 1
          )
          p1}
        
        else if (input$select2==4)
        {
          df3 <- read.csv("~/calpred.csv")
          df6 <- read.csv("~/valpred.csv")
            p1 <- gridExtra::grid.arrange(ggplot(df3) + geom_point(aes(x = df3$YREF, y = df3$predYsvm), color = "black", pch=1, size=5) + geom_smooth(aes(x = df3$YREF, y = df3$predYsvm), method = "lm", se = TRUE) +
                                              theme_bw() + theme(                              
                                                  axis.title.x = element_text(face="bold", color="black", size=16),
                                                  axis.title.y = element_text(face="bold", color="black", size=16),
                                                  plot.title = element_text(face="bold", color = "black", size=16),
                                                  legend.position=c(1,1),
                                                  legend.justification=c(1,1)) +
                                            labs(x="Reference Analysis", 
                                                 y = "Model Prediction", 
                                                 title= "Calibration Plot"),
                                          ggplot(df6) + geom_point(aes(x = df6$YREF, y = df6$predYsvm1), color = "black", pch=1, size=5) + geom_smooth(aes(x = df6$YREF, y = df6$predYsvm1), method = "lm", se = TRUE) +
                                              theme_bw() + theme(                              
                                                  axis.title.x = element_text(face="bold", color="black", size=16),
                                                  axis.title.y = element_text(face="bold", color="black", size=16),
                                                  plot.title = element_text(face="bold", color = "black", size=16),
                                                  legend.position=c(1,1),
                                                  legend.justification=c(1,1)) +
                                            labs(x="Reference Analysis", 
                                                 y = "Model Prediction", 
                                                 title= "External Validation Plot"), nrow = 1
            )
            p1
        }
        else if (input$select2==5)
        {
          df3 <- read.csv("~/calpred.csv")
          df6 <- read.csv("~/valpred.csv")
            p1 <- gridExtra::grid.arrange(ggplot(df3) + geom_point(aes(x = df3$YREF, y = df3$predYsvm), color = "black", pch=1, size=5) + geom_smooth(aes(x = df3$YREF, y = df3$predYsvm), method = "lm", se = TRUE) +
                                              theme_bw() + theme(                              
                                                  axis.title.x = element_text(face="bold", color="black", size=16),
                                                  axis.title.y = element_text(face="bold", color="black", size=16),
                                                  plot.title = element_text(face="bold", color = "black", size=16),
                                                  legend.position=c(1,1),
                                                  legend.justification=c(1,1)) +
                                            labs(x="Reference Analysis", 
                                                 y = "Model Prediction", 
                                                 title= "Calibration Plot"),
                                          ggplot(df6) + geom_point(aes(x = df6$YREF, y = df6$predYsvm1), color = "black", pch=1, size=5) + geom_smooth(aes(x = df6$YREF, y = df6$predYsvm1), method = "lm", se = TRUE) +
                                              theme_bw() + theme(                              
                                                  axis.title.x = element_text(face="bold", color="black", size=16),
                                                  axis.title.y = element_text(face="bold", color="black", size=16),
                                                  plot.title = element_text(face="bold", color = "black", size=16),
                                                  legend.position=c(1,1),
                                                  legend.justification=c(1,1)) +
                                            labs(x="Reference Analysis", 
                                                 y = "Model Prediction", 
                                                 title= "External Validation Plot"), nrow = 1
            )
            p1}
        
        else if (input$select2==6)
        {
          df3 <- read.csv("~/calpred.csv")
          df6 <- read.csv("~/valpred.csv")
            p1 <- gridExtra::grid.arrange(ggplot(df3) + geom_point(aes(x = df3$YREF, y = df3$predYsvm), color = "black", pch=1, size=5) + geom_smooth(aes(x = df3$YREF, y = df3$predYsvm), method = "lm", se = TRUE) +
                                              theme_bw() + theme(                              
                                                  axis.title.x = element_text(face="bold", color="black", size=16),
                                                  axis.title.y = element_text(face="bold", color="black", size=16),
                                                  plot.title = element_text(face="bold", color = "black", size=16),
                                                  legend.position=c(1,1),
                                                  legend.justification=c(1,1)) +
                                            labs(x="Reference Analysis", 
                                                 y = "Model Prediction", 
                                                 title= "Calibration Plot"),
                                          ggplot(df6) + geom_point(aes(x = df6$YREF, y = df6$predYsvm1), color = "black", pch=1, size=5) + geom_smooth(aes(x = df6$YREF, y = df6$predYsvm1), method = "lm", se = TRUE) +
                                              theme_bw() + theme(                              
                                                  axis.title.x = element_text(face="bold", color="black", size=16),
                                                  axis.title.y = element_text(face="bold", color="black", size=16),
                                                  plot.title = element_text(face="bold", color = "black", size=16),
                                                  legend.position=c(1,1),
                                                  legend.justification=c(1,1)) +
                                            labs(x="Reference Analysis", 
                                                 y = "Model Prediction", 
                                                 title= "External Validation Plot"), nrow = 1
            )
            p1}
    })    
    
    output$plot2 <- renderPlot(
        op3()
    )
}

# Run the app ----
shinyApp(ui = ui, server = server)

