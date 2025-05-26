#   ____________________________________________________________________________
#   Server                                                                  ####

library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(DT)
library(shinyBS)
library(data.table)
library(shiny)
library(openxlsx)
library(shinythemes)
library(caret)
library(rJava)
library(xlsx)
library(xlsxjars)
library(kernlab)
library(factoextra)
library(scales)
library(randomForest)

# Carga el modelo
model_1 <- readRDS("modelo_RF_ILRdetection.rds")
model_2 <- readRDS("modelo_RF_ILRidentification.rds")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Variables for testing                                                   ####

#CT <- 36047019500
#CT <- 36061010200
#input <- NULL
#input$addr <- "51 Kent Ave, Brooklyn, NY 11249"
#addr <- data.frame(lat = 40.7223311,
#                   lon = -73.9614495,
#                   GEOID = 36061010200)
#addrSearch <- "120 Willoughby Ave, Brooklyn"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Pretty-print function                                                   ####

format_metric <- function(x, type) {
    switch(type,
           currency = paste0("$", 
                             formatC(round(as.numeric(x), 0), 
                                     big.mark = ",", 
                                     digits = nchar(as.character(round(as.numeric(x), 0)))
                             )
           ),
           real = format(round(as.numeric(x), 1), 
                         nsmall = 1, big.mark = ","),
           int = formatC(as.numeric(x), big.mark = ",", 
                         digits = nchar(as.character(x))),
           year = round(as.numeric(x),0),
           pct = paste0(format(round(as.numeric(x) * 100, 1), 
                               nsmall = 1, big.mark = ","),"%"))
}

shinyServer(function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        if (input$fileType_Input == "2") {tryCatch(
            {
                df <- openxlsx::read.xlsx(input$file1$datapath,
                                          rowNames =  input$header,
                                          sheet=as.numeric(input$sheet))
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
            
            if(input$disp == "head") {
                return(head(cbind.data.frame(sample_name=rownames(df),df[,c(1:10)])))
            }
            else {
                return(cbind.data.frame(sample_name=rownames(df),df))
            }
            
            
        } else {
            
            
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    df <- read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            
            if(input$disp == "head") {
                return(head(cbind.data.frame(sample_name=rownames(df),df[,c(1:10)])))
            }
            else {
                return(cbind.data.frame(sample_name=rownames(df),df))
            }}
        
    })
    
    contents1 <- reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        if (input$fileType_Input == "2") {tryCatch(
            {
                df <- openxlsx::read.xlsx(input$file1$datapath,
                                          rowNames =  input$header,
                                          sheet=as.numeric(input$sheet))
                Output <- cbind.data.frame(`Sample Name` = rownames(df),`Is juice adulterated?`=predict(model_1,df))
                rellenar <- data.frame(Name_Sample=rownames(df),Prediccion=NA)
                for (i in 1:length(df[,1])){ 
                  if(predict(model_1,df[i,])=="ILR"){
                    rellenar$Prediccion[i] <- predict(model_2,df[i,])}
                  else {
                    rellenar$Prediccion[i] <- "Not applicable" }
                }
                rellenar$Prediccion <- ifelse(rellenar$Prediccion == 1, "Dies",
                                         ifelse(rellenar$Prediccion == 2, "Eth",
                                        ifelse(rellenar$Prediccion == 3, "Gas",
                                               ifelse(rellenar$Prediccion == 4, "Ker", "Not applicable"))))
                tabla_final <- cbind.data.frame(Output,`Predicted percentage of adulteration` = rellenar$Prediccion)
  
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
            print(tabla_final)            
            
        } else {
            
            
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    df <- read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
                    df <- openxlsx::read.xlsx(input$file1$datapath,
                                              rowNames =  input$header,
                                              sheet=as.numeric(input$sheet))
                    Output <- cbind.data.frame(`Sample Name` = rownames(df),`Is juice adulterated?`=predict(model_1,df))
                    rellenar <- data.frame(Name_Sample=rownames(df),Prediccion=NA)
                    for (i in 1:length(df[,1])){ 
                      if(predict(model_1,df[i,])=="ILR"){
                        rellenar$Prediccion[i] <- predict(model_2,df[i,])}
                      else {
                        rellenar$Prediccion[i] <- "Not applicable" }
                    }
                    rellenar$Prediccion <- ifelse(rellenar$Prediccion == 1, "Dies",
                                                  ifelse(rellenar$Prediccion == 2, "Eth",
                                                         ifelse(rellenar$Prediccion == 3, "Gas",
                                                                ifelse(rellenar$Prediccion == 4, "Ker", "Not applicable"))))
                    tabla_final <- cbind.data.frame(Output,`Predicted percentage of adulteration` = rellenar$Prediccion)


                    
                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            print(tabla_final)
        }
        
    })
    
    data <- openxlsx::read.xlsx("test_ILRs.xlsx", sheet=1)
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(paste0("test_ILRs", Sys.Date(), sep=""),".xlsx",sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data, file)
      }
    )
    
    # Cargamos los datos
    ftir <- readRDS("ILs_Data.R")
    
    # Se realiza el análisis de componentes principales
    pca_ims <- prcomp(ftir[,-c(1)], scale = F)
    # Se representa el porcentaje de variabilidad explicada por cada PC
    grafico_3 <- fviz_eig(pca_ims, addlabels = TRUE, main = "")
    # Se calculan las puntuaciones obtenidas por las muestras para las 10 primeras PCs
    scores_pca <- cbind.data.frame(predict(pca_ims)[,1:10],Grupo=ftir$Grupo)
    
    
    output$ui_body_0 <- renderUI({
        if (input$submitbutton>0) {
            tabItem(tabName = "info",
                    fluidRow(
                        column(width = 11,
                               h1(tags$a(tags$img(src="images/regresion.png", height = '100', width = '120')),"Supervised Analysis",tags$a(tags$img(src="images/clasificacion.png", height = '100', width = '120')))
                        )
                        ,box(
                            title = "RF model",
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            status = "primary",
                            solidHeader = FALSE,
                            tags$p("Information"
                            ), 
                            tags$ol(
                                tags$li("Predictions to identify the presence of ILRs in fire debris are performed using a previously trained RF model."),
                                tags$li("The accuracy for the RF model was 100% in both the test and training sets."),
                                tags$li("The identification of the type of ILRs is also performed using a previously trained RF model. This model obtained an accuracy of 96.3% for the test set."),
                                tags$li("The data have been published in the article entitled:\"Smart Portable Electronic Nose System in Combination with Machine Learning Algorithms for the Intelligent Discrimination of Ignitable Liquids Residues in Interfering Matrix.\"")
                            ),
                            br()
                        )
                    )
            )
        }
    })
    
    
    
    output$ui_body <- renderUI({
        if (input$submitbutton>0) {
            tabItem(tabName = "info",
                    fluidRow(
                        column(width = 11,
                               h1(tags$a(tags$img(src="images/ojo.png", height = '100', width = '120')),"Exploratory Analysis"),
                               h4("Note: The figures of the exploratory analysis have been generated using all the samples previously analyzed as well as those uploaded by the user, appearing in black.")
                        )
                        ,box(
                            title = "Hierarchical cluster analysis",
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            status = "primary",
                            solidHeader = FALSE,
                            tags$p("Information"
                            ), 
                            tags$ol(
                                tags$li("The dendogram obtained from the HCA analysis is presented using the euclidean distance and the Ward method."),
                                tags$li("Samples uploaded by the user are colored black.")
                            ),
                            br(),
                            renderPlot({
                              req(input$file1)  # Verificar que se haya cargado un archivo
                              
                              # Obtener el archivo cargado por el usuario
                              file <- input$file1
                              
                              # Leer el archivo en un data frame
                              user_data <- openxlsx::read.xlsx(input$file1$datapath,
                                                               rowNames =  input$header,
                                                               sheet=as.numeric(input$sheet))
                              data <- cbind.data.frame(Grupo=c(rep("Unknown",dim(user_data)[1])),user_data)
                              ftir <- rbind.data.frame(ftir,data)
                              ftir$Grupo <- as.factor(ftir$Grupo)
                              
                              # Se realiza el análsisis HCA usando la distancia de manhattan y el método de Wards. De esta forma se colorean las muestras en función del grupo
                              dendo <- as.dendrogram(hclust(d=dist(ftir[,-c(1)],method="euclidean"), method="ward"))
                              # Definir colores
                              hex <- c(hue_pal()(5), "black")
                              names(hex) <- levels(ftir$Grupo)  # Asignar nombres a los colores
                              colors <- hex[as.character(ftir$Grupo)]
                              
                              # Asegurarse de que los colores están ordenados de acuerdo al dendograma
                              colores <- colors[order.dendrogram(dendo)]
                              
                              
                              
                              # Asegurarse de que los colores están ordenados de acuerdo al dendograma
                              ordered_colors <- colors[order.dendrogram(dendo)]
                              # Se realiza la representación del dendograma
                              fviz_dend(x = hclust(d=dist(ftir[,-c(1)],method="euclidean"), method="ward"), k=1, color_labels_by_k=T,lwd=1, cex=0.6,type = "rectangle",label_cols = colores,rect =F, rect_fill = F)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank())                            }, width = 1600, height = 600),
                            br(),
                            br(),
                            tags$hr()
                        )
                    )
            )
        }
    })
    
    
    output$ui_body_2 <- renderUI({
        if (input$submitbutton>0) {
            tabItem(tabName = "info",
                    fluidRow(
                        box(
                            title = "Principal Component Analysis (PCA)",
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            status = "primary",
                            solidHeader = FALSE,
                            tags$p("Informacion"
                            ), 
                            tags$ol(
                                tags$li("The graph shows the scores obtained by the samples as a function of the first 3 principal components."),
                                tags$li("The graph is interactive, so you can zoom in, check the name of the sample, delete groups by clicking on the legend, etc."),
                                tags$li("Samples uploaded by the user are colored black.")
                            ),
                            br(),
                            renderPlotly({
                              req(input$file1)  # Verificar que se haya cargado un archivo
                              
                              # Obtener el archivo cargado por el usuario
                              file <- input$file1
                              
                              # Leer el archivo en un data frame
                              user_data <- openxlsx::read.xlsx(input$file1$datapath,
                                                               rowNames =  input$header,
                                                               sheet=as.numeric(input$sheet))
                              data <- cbind.data.frame(Grupo=c(rep("Unknown",dim(user_data)[1])),user_data)
                              ftir <- rbind.data.frame(ftir,data)
                              ftir$Grupo <- as.factor(ftir$Grupo)
                              # Se realiza el análisis de componentes principales
                              pca_ims <- prcomp(ftir[,-c(1)], scale = T)
                              # Se representa el porcentaje de variabilidad explicada por cada PC
                              grafico_3 <- fviz_eig(pca_ims, addlabels = TRUE, main = "FT-IR")
                              # Se calculan las puntuaciones obtenidas por las muestras para las 10 primeras PCs
                              scores_pca <- cbind.data.frame(predict(pca_ims)[,1:10],Grupo=ftir$Grupo)
                              # Define los colores personalizados para cada nivel del factor "grupo"
                              colores <- c(hue_pal()(5),"black") # Aquí puedes modificar los colores según tus preferencias
                              
                              # Crea una función que asigna los colores personalizados a cada nivel del factor
                              color_mapping <- function(grupo) {
                                return(colores[as.numeric(grupo)])
                              }
                              
                              plot_ly(data=scores_pca, y=~PC2, x=~PC1, z=~PC3, color=~Grupo,text=rownames(scores_pca),marker = list(size = 6, color = ~color_mapping(Grupo)))%>% layout(
                                title = "Puntuaciones PCA",
                                scene = list(
                                  xaxis = list(title = "PC1 (12.3%)"),
                                  yaxis = list(title = "PC2 (6.7%)"),
                                  zaxis = list(title = "PC3 (3.7%)")))

                            }),
                            br(),
                            tags$hr()
                        )
                    )
            )
        }
    })
    
    
    # Status/Output Text Box
    output$contents1 <- renderPrint({
        if (input$submitbutton>0) { 
            isolate("Calculation completed.") 
        } else {
            return("Waiting for calculation.")
        }
    })
    
    # Status/Output Text Box
    output$contents2 <- renderPrint({
        if (input$submitbutton>0) { 
            isolate("The result of its prediction using the RF model is:") 
        }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(contents1()) 
        } 
    })
    
})