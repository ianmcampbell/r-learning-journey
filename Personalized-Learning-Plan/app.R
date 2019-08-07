library(shiny)
library(RCurl)
library(data.table)
source("URL-Encode.R")
lesson_table <- fread("LessonTable.txt")
ui <- fluidPage(
    uiOutput("page_output")
)

server <- function(input, output, session) {


output$page_output <- renderUI({

    query <- parseQueryString(session$clientData$url_search)
    learner_name <- query[["userid"]]
    if(length(learner_name) == 0)
       {title_string <- "Personalized Learning"}
    else
    {title_string <- paste0(urlsafebase64decode(learner_name),"'s")}
    lesson_url_list <- query[["lessonkey"]]
    if(length(lesson_url_list) == 0)
    {lesson_list <- 101:111}
    else
    {lesson_list <- decode_lessons(lesson_url_list)}
    student_lessons <- lesson_table[lesson_key %in% lesson_list, ]
    
    
    title <- paste0(title_string," Curriculum for the August 2019 R Workshop")
        fluidRow(column("",width=2),column(
        titlePanel(title),

        tags$div("It looks like you are just starting out with R and RStudio. I’ve created a curriculum for you that takes you from basic R syntax all the way to visual representations of your data and beyond. By following the steps outlined below, you can become very good at syntactical rules that are consistent no matter what you do down the line.",

    tags$ol(
        tags$li("Please inspect your packet. It contains Hadley Wickham’s ",tags$a(href="https://vita.had.co.nz/papers/tidy-data.pdf", "seminal article")," about “tidy data” (a concept that is at the heart of good coding in R), which you can read at your leisure, and numerous cheat sheets that are loosely arranged from least to most advanced, to which you can refer as you follow the instructions below."),
        tags$li("Open RStudio and follow along as you watch",tags$a(href="https://storage.googleapis.com/braunsb-media/Quick R Help/Customizing RStudio.mp4", "Customizing RStudio")," (below). I have assumed that the viewer knows nothing about R and RStudio except how to launch RStudio. Go ahead and set up RStudio as you like.")
    ),
    tags$div(tags$video(src = "https://storage.googleapis.com/braunsb-media/Quick R Help/Customizing RStudio.mp4", type = "video/mp4", autoplay = NA, controls = TRUE, width = "50%"),align="center"),
    tags$br(),
    tags$br(),
    "Having completed these steps, you are ready to start coding. There are two alternatives here for learning R basics. Please use whichever works best for you. The first one has accompanying videos for many of the lessons. They go fast and can be helpful at anchoring concepts in your mind—especially if you are a visual learner.",
    tags$h3("Browser Based Learning Curriculum"),
    "Here is a list of lessons that have been chosen based on your feedback. They can be completed in any modern browser. Your progress is saved and you can return at any time.",
    tags$br(),
    tags$div(
    apply(cbind(seq(1,nrow(student_lessons)),student_lessons[,list(title,url)]),1,function(x){list(tags$a(href=x[3], paste(c("Lesson ",x[1],": ",x[2]),collapse = ""), target = "newtab"),tags$br())})
    , style = "font-size:20px; margin-left:5%"),
    tags$h3("Another Alternative"),
    "There is a package you can use to learn R called adventr. adventr is a tutorial package created in R by Andy Field to accompany his rather unusual book starring a guy named Zach and a sarcastic cat. This tutorial uses the tidyverse rather heavily and gets into statistics earlier than the swirl tutorials do. You might find that this suits you better—and then again, you might find yourself lost and frustrated by it, in which case I suggest you go back to using swirl.
    Here is how to install and use adventr:",
    tags$ol(
        tags$li("At the console, install the package by typing:",tags$br(),code("install.packages(“devtools”) # unless you have done this already",tags$br(),code("library(devtools)"),tags$br(),code("install_github(“profandyfield/adventr”)"))),
        tags$li("Put the package in your library by typing:",tags$br(),code("library(adventr)")),
        tags$li("Now run the first lesson by typing:",tags$br(),code('learnr::run_tutorial("adventr_02", package = "adventr")'))
    ),
    "No idea why it’s 02 instead of 01. But it is. Please contact me if these steps don’t work for you.
    4.	You can either read the background material about Andy Field’s book (a comic book for teaching R and statistics in R? Yes.) by following its link or read the information on the html page that opens and press continue or next topic. Along the way, you will encounter boxes where you can enter R code and see how it works. Once you get past adventr_02, the statistics begin, so you might want to stop after that. If you plan to attend the R statistics workshops and you like Any Field’s style of teaching, go on with the lessons.",
    tags$h3("For Next Time"),
    "For next time, please ask yourself precisely how you will be using R in your workflow and, when you register, enter the specific things you need to know to be able to accomplish your goals.
    And finally, if you haven’t already, consider joining the CHOP R User Group, where you will find cookies (the edible ones), coffee, conversations, presentations, and help with your code. We have a newsletter to announce upcoming gatherings (usually held semimonthly) and an active slack user group.",
    tags$a(href="https://bit.ly/chop-r)",tags$img(src = "chop-r.gif",width = "225px", height="150px", style="display: block; margin-left: auto; margin-right: auto;")),
    tags$img(src = "footer.png")
    , style = "font-size:15px")
    , width=8),column("",width=2))
})
}

shinyApp(ui = ui, server = server)
