library(shiny)
library(RCurl)
library(htmlwidgets)
library(data.table)
#Source functions for URL encoding and decoding
source("URL-Encode.R")
#Load the lesson table from disk
lesson_table <- fread("LessonTable.txt")
selected_keys <- reactiveValues()
selected_keys$logical <- rep(TRUE, nrow(lesson_table))

#Custom sortable list function for ordering lessons
sortableList <- function(inputId, value) {
    tagList(
        singleton(tags$head(tags$script(src= "Sortable.js"))),
        singleton(tags$head(tags$script(src = "sortableList.js"))),
        tags$div(id = inputId,class = "sortableList list-group",
                 tagList(sapply(value,function(x){
                     tags$div(class="list-group-item","data-id"=x,x)
                 },simplify = F)))
    )
}

ui <- fluidPage(
    uiOutput("page_output")
)

server <- function(input, output,session) {

        output$page_output <- renderUI({

        fluidRow(column("",width=2),column(
        titlePanel("Custom Learning Plan Generation"),
        tags$h3("Learner Name:"),
        textInput("name", label = ""),
        tags$br(),       
        tags$h3("Lessons to Include:"),
        
        #Generate list of check boxes based on the lesson table
        apply(lesson_table[,list(lesson_key,title)],1,function(x){list(checkboxInput(x[1],x[2],value = TRUE))}),
        actionButton("make_list", "Populate List"),
        tags$br(),
        tags$br(),
        #Generate re-orderable list of lessons based on check boxes 
        tags$h3("Reorder Lessons:"),
        fluidRow(column(
            sortableList('sorted_list',apply(lesson_table[selected_keys$logical,list(title,lesson_key)],1,function(x){paste0(x[1],": ",x[2],collapse="")}))
                 ,width=6)),
        actionButton("gen", "Generate URL"),
        tags$br(),
        tags$br(),
        tags$h3("Custom URL:"),
        textInput("url_holder", label = "", value = "",width="70%")
        , width=8),column("",width=2))
    })
    
    observeEvent(input$make_list,{
        newly_selected_keys <- vector()
        for(i in lesson_table$lesson_key) {
            newly_selected_keys <- c(newly_selected_keys,input[[as.character(i)]])
        }
        selected_keys$logical <- newly_selected_keys
    })   
        
    observeEvent( input$gen,{
        #Encode the learner's name to Base64
        encoded_name <- urlsafebase64encode(input$name)
        
        filtered_lesson_table <- lesson_table
        
        #Remove lesson title leaving only the lesson keys
        sorted_list <- as.numeric(gsub(".*: +","",input$sorted_list))
        if(is.na(sorted_list)){sorted_list <- filtered_lesson_table[,lesson_key]}
        encoded_list <- encode_lessons(sorted_list)
        if(nchar(encoded_name) > 1){name_url_element <- paste0("userid=",encoded_name,"&")} else {name_url_element <- ""}
        URL <- paste0("http://shinyserver.tk:3838/Personalized-Learning-Plan/?",name_url_element,"lessonkey=",encoded_list,collapse = "")
        updateTextInput(session, "url_holder", value = URL)
    }
    )
}

shinyApp(ui = ui, server = server)
