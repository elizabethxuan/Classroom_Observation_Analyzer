#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(leaflet)
library(fmsb)
library(DT)
#setwd('~/Desktop/Classroom Observation Analyzer')
raw.df <- read_excel("Observation Criteria Rubric Data.xlsx") %>% data.frame() 
colnames(raw.df)=c('Section','Question','A: Excellent','B: Adequate','C: Needs Improvement','D: No evidence')
sections <- raw.df %>% group_by(Section) %>%  summarise(n=n())


totalpages = 38

# function to convert choice to grade
grade <- function(x){
  switch(x,
         'A'=100,
         'B'=80,
         'C'=60,
         'D'=40)
}

# function to get the comment part from the chosen string
comment <- function(chosen){
  l=length(strsplit(chosen,'[:]')[[1]])
  return(paste(strsplit(chosen,'[:]')[[1]][3:l]))
}

addOptions <- function(row){
  options=c()
  for (i in 1:4){
    options=c(options,paste0(names(row)[i],':',row[i]))
  }
  options
}


nextPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
}
prevPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
}
wrapPage <- function(title, page, button_left = NULL, button_right = NULL) {
  tabPanel(
    title = title, 
    fluidRow(
      column(12, page)
    ), 
    fluidRow(
      column(6, button_left),
      column(6, button_right)
    )
  )
}
wizardUI <- function(id, pages, doneButton = NULL) {
  stopifnot(is.list(pages))
  n <- length(pages)
  
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has next; last page only prev + done
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else doneButton
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
  }
  
  # Create tabsetPanel
  # https://github.com/rstudio/shiny/issues/2927
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
}

wizardServer <- function(id, n) {
  moduleServer(id, function(input, output, session) {
    changePage <- function(from, to) {
      observeEvent(input[[paste0("go_", from, "_", to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })  
    }
    ids <- seq_len(n)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-n], function(i) changePage(i, i + 1))
  })
}

ui <- fluidPage(
  titlePanel("Classroom Observation Analyzer"),
  sidebarLayout(
  sidebarPanel(
    textInput("class", "Class:"),
    textInput("section", "Section:"),
    textInput("obser", "Observer:"),
    textInput("obsee", "Instructor:"),
    textInput("date", "Class Date:"),
    textInput("start", "Start Time:"),
    textInput("arrival", "Arrival Time:"),
    textInput("topic", "Topic Covered:"),
    radioButtons("mod","Instruction Mode:",c('online','in-person')),
  
  ),
  mainPanel(
    wizardUI(
      id = "evaluation", 
      pages = lapply(1:totalpages, function(i){
        tagList(
          raw.df[i,1],
          radioButtons(paste0('q',i),
                       paste0(i,'. ',raw.df[i,2]),
                       addOptions(raw.df[i,3:6]))
        )
      }
      ),
      doneButton = actionButton("done", "Submit")
    ),
    tabsetPanel(
      id = 'result',
      type = 'hidden',
      tabPanel('p1'),
      tabPanel('p2',
               plotOutput('plot2'),
               downloadButton("dlchart","Download Instructor-Skill Analysis Chart"),
               downloadButton("dlrep","Download Class Observation Report"),)
    )
  )
  )
)
server <- function(input, output, session) {
  wizardServer("evaluation", totalpages)
  
  observeEvent(input$done, 
               updateTabsetPanel(inputId = "result", selected = 'p2')
  )
  
  
  # collect choice results and create a reactive table for radarchart
  df2 =  reactive({
    scs = as.data.frame(do.call(rbind,lapply(1:totalpages,function(i){
      strsplit(input[[paste0('q',i)]],'[:]')[[1]][1]%>% grade()
    }
    ))) %>% rename('Score'='V1') %>% cbind(raw.df[1:totalpages,1:2]) %>%
      group_by(Section) %>% 
      summarise(avg.score = mean(Score,na.rm=T)) %>% t() %>%
      as.data.frame(stringsAsFactors = FALSE)
    df.toplot = scs[2,]
    colnames(df.toplot) = scs[1,]
    return(rbind(rep(100,ncol(df.toplot)),rep(0,ncol(df.toplot)),df.toplot)
           %>% mutate_all(function(x)as.numeric(x)))
  })
  
  
  # raderchart plot
  output$plot2 <- renderPlot({
    df2()  %>%
     radarchart(  pfcol=rgb(0.2,0.5,0.5,0.5) ,vlcex=0.8)
  })
  
  # plot to be downloaded
  p <- reactive({
    df2()  %>%
      radarchart(  pfcol=rgb(0.2,0.5,0.5,0.5) ,vlcex=0.8)
  })
  
  
  output$dlrep <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "test.Rmd")
      file.copy("test.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(class = input$class, section =input$section, obser=input$obser,
                     obsee = input$obsee, date = input$date, start=input$start,
                     arrival = input$arrival, topic=input$topic, mod=input$mod,
                     Qs1 = paste(comment(input$q1),comment(input$q2),comment(input$q3),comment(input$q4),
                                 comment(input$q5),comment(input$q6),comment(input$q7)),
                     Qs2 = paste(comment(input$q8),comment(input$q9),comment(input$q10),comment(input$q11),
                                 comment(input$q12),comment(input$q13)),
                     Qs3 = paste(comment(input$q14),comment(input$q15),comment(input$q16),comment(input$q17),
                                 comment(input$q18),comment(input$q19)),
                     Qs4 = paste(comment(input$q20),comment(input$q21),comment(input$q22),comment(input$q23),
                                 comment(input$q24),comment(input$q25)),
                     Qs5 = paste(comment(input$q26),comment(input$q27),comment(input$q28),comment(input$q29)),
                     Qs6 = paste(comment(input$q30),comment(input$q31),comment(input$q32),comment(input$q33)),
                     Qs7 = paste(comment(input$q34),comment(input$q35),comment(input$q36),comment(input$q37),
                                 comment(input$q38))
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  # download radarchart
  output$dlchart <- downloadHandler(
    filename = function() { 'Instructor-Skill Analysis Chart.png' },
    content = function(file) {
      png(file)
      print(p())
      dev.off()
    })
  
 
  
}


shinyApp(ui, server)
