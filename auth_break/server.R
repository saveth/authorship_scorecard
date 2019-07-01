## title: "Authorship Scorecard"
## author: "Savet Hong"
## date: "July 1, 2019"
## Purpose: Build Interactive Scorecard
##
##  NOTE: The same as auth.R but broken into ui and server scrip for ease of code
##  editing
##################################################
library(tidyverse)
library(writexl)
library(stringr)
library(shiny)
library(shinyWidgets)
library(DT)
library(rhandsontable)

load("../auth.Rdata")
# Define Server for Authorship Scorecard application 
shinyServer(function(input, output) {

    #Messages
    output$usr_out <- eventReactive(input$submit1,{
        print(paste0( "Thank you ",input$usr_in, ". Please go to the Manuscript tab."))
    })
    
    output$score_msg <- eventReactive(input$runButton, {
        print("\nIf you are satisfied please go to the Rank tab. \nOtherwise, make changes and click on the 'Add Score Value' again. ")
    })
    
    output$manuUI <- renderUI({
        papers <- c(names(manu_list), "Other") 
        selectInput("paper", "", papers, 1)
        #Select a Manuscript:
    })
    
    #authdf <- reactive(input)
    
    output$authlist <- renderTable({
        input$auth 
    }, colnames = FALSE)
    
    # BUild the dataset
    df1 <- reactive({
        #Scoring table
        Section <- c('Accreditation and EES', 'Data UI & Splashpage', 'HQ & Facilitator', 
                     'Models', 'Qualitative Workgroup', 'Research tasks', 'Sim UI', 'Team Time Report', 
                     'Development', 'Analysis', 'Manuscript', 'Managing Submission Process', 
                     'Literature search', 'Institutional Review Board', 
                     'Data Collection and Preparation (DCP)', 'Other administrative duties')
        usr_sect_wgt <- c(input$ees, input$dui, input$hq, input$model, input$qual,
                          input$research, input$sui, input$ttr, 
                          input$dev, input$Analysis, input$Manuscript, input$msprocess, 
                          input$lit, input$irb, input$dcp, input$oad)
        
        dat1 <- data.frame(Section, usr_sect_wgt, stringsAsFactors = FALSE) %>%
            arrange(desc(usr_sect_wgt)) %>%
            add_row(Section = "Total", usr_sect_wgt = sum(usr_sect_wgt)) %>%
            rename(`Content Weight` = usr_sect_wgt)
        
        return(dat1)
    })
    
    output$wgt <- renderTable({
        df1()
    })
    
    df2 <- reactive({
        dat1 <- df1() %>%
            filter(Section != "Total") %>%
            rename(Category = Section) 
        #Authorlist
        dat2 <-  dat1 %>%
            cbind.data.frame(data.frame(matrix(vector(), nrow(dat1), length(input$auth),
                                               dimnames = list(c(), input$auth)),
                                        stringsAsFactors = FALSE)) %>%
            mutate_at(3:(length(input$auth)+2), as.numeric) %>%
            inner_join(df[,1:4], by = "Category") %>%
            select(names(df)[1:4], everything()) %>%
            filter(`Content Weight` != 0)
        
        
        return(dat2)
    })
    
    
    values <- reactiveValues()
    output$tbscore <- renderRHandsontable({
        rhandsontable(data = df2() %>% select(-`Content Weight`),
                      rowHeaders = NULL,
                      contextMenu = FALSE,
                      #width = 600,
                      height = 700)
    })
    
    observeEvent(input$runButton,{
        values$data <- hot_to_r(input$tbscore) 
    })
    df3 <- reactive({
        dat1 <- df1() %>%
            filter(Section != "Total") %>%
            rename(Category = Section) 
        
        values$data %>%
            select(-`Points Possible`) %>%
            gather(Author, score,  -Eligibility, -Category, -Subcategory) %>%
            left_join(dat1) %>%
            mutate(Author = sub("\\.\\.", ", ", Author),
                   Author = sub("\\.", " ", Author),
                   score = score * (`Content Weight`)/100) %>%
            group_by(Author) %>%
            summarise(`Total Score` = sum(score, na.rm = TRUE)) %>% 
            arrange(desc(`Total Score`))
    })
    
    output$rank <- renderDataTable({
        datatable(df3())
    })
    
    #Create Excel output file
    output$dl <- downloadHandler(
        filename = function(){paste0(input$usr_in, '_', input$paper, '.xlsx')},
        content = function(file){
            write_xlsx(list(`Category Weight` = df1(), Score = values$data, Rank = df3()), path = file)
        }
    )

})
