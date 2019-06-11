## title: "Authorship Scorecard"
## author: "Savet Hong"
## date: "May 28, 2019"
## Purpose: Build Interactive Scorecard
##
##################################################

library(tidyverse)
library(readxl)
library(stringr)
library(shiny)
library(shinyWidgets)
library(DT)
library(rhandsontable)

load("auth.Rdata")

ui <- fluidPage(
  navbarPage("Authorship Scorecard",
             tabPanel("Editor",
                      fluidRow(width = 12, offset = 1,
                               "Created by: Savet Hong for TeamPSD"),
                      hr(),
                      textInput("usr_in", ""),
                      actionButton("submit1", "Done"),
                      textOutput("usr_out")
                      ),
             tabPanel("Manuscript",
                      fluidRow(
                        sidebarLayout(
                          sidebarPanel(
                            h3("Manuscript"), 
                            uiOutput("manuUI"),
                            
                            conditionalPanel(
                              condition = "input.paper == 'Other'",
                              textInput("np", "Enter New Manuscript Name:", NULL),
                              actionButton("Addmanu", "Add Manuscript")),
                            hr(),
                            h4("Enter Weights for Content Area"),
                            dropdownButton(
                              helpText("Weights are percentage (%) contribution for each manuscript project."),
                              numericInput("Development", "Development", value = 0, min = 0, max = 100),
                              numericInput("Analysis", "Analysis", value = 0, min = 0, max = 100),
                              numericInput("Manuscript", "Manuscript", value = 0, min = 0, max = 100),
                              numericInput("msprocess", "Managing Submission Process", value = 0, min = 0, max = 100),
                              numericInput("splash", "Splashpages", value = 0, min = 0, max = 100),
                              numericInput("dui", "Data UI", value = 0, min = 0, max = 100),
                              numericInput("ttr", "Team Time Report", value = 0, min = 0, max = 100),
                              numericInput("quant", "Quant workflow", value = 0, min = 0, max = 100),
                              numericInput("hq", "HQ & Facilitator", value = 0, min = 0, max = 100),
                              numericInput("model", "Models", value = 0, min = 0, max = 100),
                              numericInput("sui", "Sim UI", value = 0, min = 0, max = 100),
                              numericInput("ees", "Accreditation and EES", value = 0, min = 0, max = 100),
                              numericInput("qual", "Qualitative Workgroup", value = 0, min = 0, max = 100),
                              status = "info", icon = icon("gear")
                            ),
                            h4("Contributor"), 
                            checkboxGroupInput("auth", "Select contributors:", 
                                               people$fname, 
                                               selected = NULL)
                            
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("weightsection",
                                       tableOutput("weight")),
                              tabPanel("People",
                                       tableOutput("authors"))
                            )
                          )
                        ))),
             tabPanel("Score",
                      mainPanel(
                        sidebarLayout(
                          sidebarPanel(actionButton("runButton","Add Score Value")),
                          mainPanel(#dataTableOutput("tbscore")
                            rHandsontableOutput("tbscore"))
                      ))
                      ),
             tabPanel("Rank",
                      dataTableOutput("rank")),
             tabPanel("Save")
             )
  
)


server <- function(input, output, session) {
  output$usr_out <- eventReactive(input$submit1,{
    print(paste0( "Thank you ",input$usr_in, ". Please go to the next page,"))
  })
  output$manuUI <- renderUI({
    papers <- c(names(manu_list), "Other") 
    selectInput("paper", "Select a Manuscript:", papers, 1)
  })
  
  # BUild the dataset
  df1 <- reactive({
    #Scoring table
    Section <- c("Development", "Analysis", "Manuscript", "Managing Submission Process",
                 "Splashpages", "Team Time Report", "Quant workflow", "HQ & Facilitator",
                 "Models", "Sim UI", "Accreditation and EES", "Qualitative Workgroup")
    usr_sect_wgt <- c(input$Development, input$Analysis, input$Manuscript, input$msprocess,
                      input$dui, input$ttr, input$quant, input$hq, input$model, input$sui,
                      input$ees, input$qual)
    dat1 <- data.frame(Section, usr_sect_wgt, stringsAsFactors = FALSE) 
      
    
    return(dat1)
  })
  
  df2 <- reactive({
    #Authorlist
    dat2 <- df1() %>%
      inner_join(df[,1:4], by = "Section") %>%
      filter(usr_sect_wgt != 0)%>%
      cbind.data.frame(data.frame(matrix(vector(), nrow(df1()), length(input$auth),
                                         dimnames = list(c(), input$auth)),
                                  stringsAsFactors = FALSE)) %>%
      mutate_at(5:(length(input$auth) + 5), as.numeric)
    
    return(dat2)
  })

  
  values <- reactiveValues()
  output$tbscore <- renderRHandsontable({
    rhandsontable(data = df2() %>% select(-usr_sect_wgt),
                  rowHeaders = NULL,
                  contextMenu = FALSE,
                  width = 600,
                  height = 300)
  })
  
  observeEvent(input$runButton,{
    values$data <- hot_to_r(input$tbscore) 
  })
  df3 <- reactive({
    values$data %>%
      select(-Points) %>%
      gather(Author, score, -Section, -Eligible, -Subsection) %>%
      mutate(Author = sub("\\.\\.", ", ", Author),
             Author = sub("\\.", " ", Author)) %>%
      group_by(Author) %>%
      summarise(`Total Score` = sum(score, na.rm = TRUE)) %>% 
      arrange(desc(`Total Score`))
  })
  
  output$rank <- renderDataTable({
    datatable(df3())
  })
  
  #Create Excel output file


}

# Run the application 
shinyApp(ui = ui, server = server)