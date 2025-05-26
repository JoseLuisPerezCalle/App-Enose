#   ____________________________________________________________________________
#   UI                                                                      ####

library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(shinydashboard)




### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue




ui <- navbarPage(title = "AGR-291",
                   theme = "style/style.css",
                   fluid = TRUE, 
                   collapsible = TRUE,

                   
                   # ----------------------------------

                   tabPanel("Detection and identification ILRs", "The prediction aims to detect the presence and type of ignitable liquid residues (ILRs) in fire debris. The data were generated from the analysis of samples using a portable electronic nose based on sensors. The data matrix generated from the sample analysis was coupled with various machine learning algorithms, and the prediction of both the presence and identification of ILR types is performed using a Random Forest (RF) model. Detailed information about the models and samples used in this application can be found in the article titled \"Smart Portable Electronic Nose System in Combination with Machine Learning Algorithms for the Intelligent Discrimination of Ignitable Liquids Residues in Interfering Matrix.\" Additionally, a sample file (download button) has been included, which can be uploaded to the webpage to verify functionality.",
                              
                              # App title ----
                              titlePanel("Data upload"),
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                  downloadButton("downloadData", label = "Download example"),
                                  radioButtons(
                                    "fileType_Input",
                                    label = h4("Choose the type of file"),
                                    choices = list(".csv/txt" = 1, ".xlsx" = 2),
                                    selected = 2,
                                    inline = TRUE
                                  ),
                                  
                                  # Input: Select a file ----
                                  fileInput("file1", "Upload the excel file",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv",
                                                       '.xlsx')),
                                  
                                  # Horizontal line ----
                                  tags$hr(),
                                  h2(actionButton("submitbutton","Analyze"),align="center"),
                                  tags$hr(),
                                  
                                  # Input: Checkbox if file has header ----
                                  checkboxInput("header", "Header", TRUE),
                                  
                                  # Input: Select separator ----
                                  radioButtons("sep", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","),
                                  
                                  # Input: Select quotes ----
                                  radioButtons("sheet", "Sheet",
                                               choices = c("Sheet 1" = 1,
                                                           "Sheet 2" = 2,
                                                           "Sheet 3" = 3),
                                               selected = 1),
                                  
                                  # Input: Select quotes ----
                                  radioButtons("quote", "Quote",
                                               choices = c(None = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"'),
                                  
                                  # Horizontal line ----
                                  tags$hr(),
                                  
                                  # Input: Select number of rows to display ----
                                  radioButtons("disp", "Display",
                                               choices = c(Head = "head",
                                                           All = "all"),
                                               selected = "head")
                                  
                                  
                                  
                                ),
                                
                                # Main panel for displaying outputs ----
                                mainPanel(
                                  
                                  # Output: Data file  CV----
                                  tags$label(h5('The data uploaded are:')),
                                  tableOutput("contents"),
                                  verbatimTextOutput('contents1'),
                                  htmlOutput("ui_body_0"),
                                  verbatimTextOutput('contents2'),
                                  tableOutput("tabledata"),
                                  htmlOutput("ui_body"),
                                  htmlOutput("ui_body_2")
                                )))

                   
)
