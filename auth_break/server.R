## title: "Authorship Scorecard"
## Code written by:"Savet Hong"
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
    usname <- eventReactive(input$submit1,{
        input$usr_in
    })
    output$usr_out <- renderText({
        paste0( "Thank you ","<font color=\"#0083BE\"><b>", usname(), "</b></font>", ". Please go to the Manuscript tab.")
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
                     'Models', 'Qualitative Workgroup', 'Research tasks', 'Sim UI', 
                     'Team Time Report'#, 
                     # 'Development', 'Analysis', 'Manuscript', 'Managing Submission Process', 
                     # 'Literature search', 'Institutional Review Board', 
                     # 'Data Collection and Preparation (DCP)', 
                     # 'Other administrative duties'
                     )
        usr_sect_wgt <- c(input$ees, input$dui, input$hq, input$model, input$qual,
                          input$research, input$sui, input$ttr#, 
                          # input$dev, input$Analysis, input$Manuscript, input$msprocess, 
                          # input$lit, input$irb, input$dcp, input$oad
                          )
        
        dat1 <- data.frame(Section, usr_sect_wgt, stringsAsFactors = FALSE) %>%
            arrange(desc(usr_sect_wgt)) %>%
            add_row(Section = "Total", usr_sect_wgt = sum(usr_sect_wgt)) %>%
            rename(`Content Weight` = usr_sect_wgt)
        
        return(dat1)
    })
    
    output$wgt <- renderTable({
        df1()
    })
    
    #BUILD REACTIVE ELIGIBILITY TABLE
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
            mutate(Total = rowSums(.[,3:(length(input$auth)+2)], na.rm = TRUE)) %>%
            inner_join(df[,1:4], by = "Category") %>%
            select(names(df)[1:4], everything()) %>%
            filter(`Content Weight` != 0) %>% 
            select(-`Content Weight`, -Eligibility)
            
        return(dat2)
    })
    
    df_elig_changes <- reactive({
      # function to compare user input to default and sum input entered into "Total" column
        if(is.null(input$tbscore)){return(df2())}
        else if(!identical(df2(),input$tbscore)){
            mytable <- as.data.frame(hot_to_r(input$tbscore))
            mytable <- mytable[1:nrow(df2()),]
            
            mytable[,(length(input$auth)+4)] <- rowSums(mytable[,4:(length(input$auth)+3)], na.rm = TRUE) 
            mytable
        }
    })
    
    color_renderer <- "
  function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.color = 'red';
  }
" #Java script entered as text to be passed through render
    
    output$tbscore <- renderRHandsontable({
        rhandsontable(data = df_elig_changes(),
                      rowHeaders = NULL,
                      #contextMenu = FALSE,
                      height = 700) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            hot_rows(fixedRowsTop = 1) %>% # fix header
            hot_col("Points Possible", renderer = color_renderer) # add color
    })
    

    #BUILD REACTIVE RESPONSIBLE TABLE
    df_res <- reactive({
        res_lim <- df %>%
            filter(Eligibility == 'Responsible') %>%
            select(Category, Subcategory, `Points Possible`)
        dat <- res_lim %>%
            cbind.data.frame(data.frame(matrix(vector(), nrow(res_lim), 
                                               length(input$auth),
                                               dimnames = list(c(), input$auth)),
                                        stringsAsFactors = FALSE)) %>%
            mutate_at(4:(length(input$auth)+3), as.numeric) %>%
            mutate(Total = rowSums(.[,4:(length(input$auth)+3)]))
        
        return(dat)
    })
    
    df_res_changes <- reactive({
        if(is.null(input$tbresp)){return(df_res())}
        else if(!identical(df_res(),input$tbresp)){
            mytable <- as.data.frame(hot_to_r(input$tbresp))
            mytable <- mytable[1:nrow(df_res()),]
            
            mytable[,(length(input$auth)+4)] <- rowSums(mytable[,4:(length(input$auth)+3)], na.rm = TRUE) 
            mytable
        }
    })
    
    output$tbresp <- renderRHandsontable({
        rhandsontable(data = df_res_changes(),
                      rowHeaders = NULL,
                      #contextMenu = FALSE,
                      #width = 600,
                      height = 700) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            hot_rows(fixedRowsTop = 1) %>% # fix header
            hot_col("Points Possible", renderer = color_renderer) # add color
    })
    
    #EXTRACT REACTIVE ELIGIBILITY/RESPONSIBLE TABLES
    values <- reactiveValues()
    
    observeEvent(input$runButton,{
        values$elig <- hot_to_r(input$tbscore)
        values$resp <- hot_to_r(input$tbresp) 
    })
    
    # CREATE RANK TABLE
    df3 <- reactive({
        dat1 <- df1() %>%
            filter(Section != "Total") %>%
            rename(Category = Section) 
        
        dfp1 <- values$elig %>%
            select(-`Points Possible`, -Total) %>%
            gather(Author, score, -Category, -Subcategory) %>%
            left_join(dat1, by = "Category") %>% # Add users weight for eligible category
            mutate(Author = sub("\\.\\.", ", ", Author),
                   Author = sub("\\.", " ", Author),
                   score = score * (`Content Weight`/100)) %>%
            group_by(Author) %>%
            summarise(Score = sum(score, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(Eligibility = 'Eligibile')
                   #eligible_wgt = input$elig) 
                   # Remove weight for eligibility relative to responsibility
            
        dfp2 <- values$resp %>%
            select(-`Points Possible`, -Total) %>%
            gather(Author, score, -Category, -Subcategory) %>%
            mutate(Author = sub("\\.\\.", ", ", Author),
                   Author = sub("\\.", " ", Author)) %>%
            group_by(Author) %>%
            summarise(Score = sum(score, na.rm = TRUE)) %>% # Calculate responsible score
            ungroup() %>%
            mutate(Eligibility = 'Responsible')
                  # eligible_wgt = (100 - input$elig))
                  # Remove weight for eligibility relative to responsibility
        
        dfp3 <- dfp1 %>%
            bind_rows(dfp2) %>%
            #mutate(Score = Score * eligible_wgt/100) %>% # add weights for eligible component
              # Remove weight for eligibility relative to responsibility
            group_by(Author) %>%
            summarise(`Total Score` = sum(Score, na.rm = TRUE)) %>% 
            arrange(desc(`Total Score`))
        
        return(dfp3)
        
    })
    
    output$rank <- renderDataTable({
        datatable(df3())
    })
    
    #Create Excel output file
    output$dl <- downloadHandler(
        filename = function(){ifelse(input$paper == 'Other', 
                                     paste0(input$usr_in, '_', input$np, '_',
                                     format(Sys.Date(), "%Y%m%d"), '.xlsx'),
                                     paste0(input$usr_in, '_', input$paper, '_',
                                            format(Sys.Date(), "%Y%m%d"), '.xlsx'))},
        content = function(file){
            write_xlsx(list(`Category Weight` = df1(), `Eligible Score` = values$elig,
                            `Responsible Score` = values$resp, Rank = df3()), 
                       path = file)
        }
    )

})
