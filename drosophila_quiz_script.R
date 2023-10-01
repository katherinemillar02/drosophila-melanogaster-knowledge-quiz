
## required shiny package
library(shiny)

## Setting the quiz questions in list 
questions <- list(
  list(
    image_filename = "drosophila_melanogaster_quiz.jpg",
    question = "What is the common name for Drosophila melanogaster?",
    choices = c("House Fly", "Fruit Fly", "Mosquito", "Butterfly"),
    correct = "Fruit Fly"
  ),
  
  list(
    image_filename = "sex_chromosomes_quiz.png",
    question = "Which chromosome/s determines the sex of Drosophila melanogaster?",
    choices = c("Both X and Y chromosomes", "Y chromosome", "X chromosome"),
    correct = "X chromosome"
  ), 
  
  list(
    image_filename = "drosophils_discovery.jpg",
    question = "Who first discovered Drosophila melanogaster as a model organism?",
    choices = c("Emile Mapus", "Thomas Hunt Morgan", "Gregor Mendel", "Nettie Stevens"),
    correct = "Thomas Hunt Morgan"), 
  
  list(
    image_filename = "white_eyed_mutant.webp",
    question = "What is a white eyed mutant in Drosophila melanogaster",
    choices = c("A fly that has red eyes, instead of white", "A fly that has white eyes, instead of red", "A fly that has scarlet eyes, instead of white"),
    correct = "A fly that has white eyes, instead of red"), 
  
  
  list(
    image_filename = "drosophila_genome_quiz.jpeg",
    question = "What features characterise the genome of drosophila melanogaster?",
    choices = c("Two pairs of chromosomes", "Four pairs of chromosomes", "Eight pairs of chromosomes", "Sixteen pairs of chromosomes"),
    correct = "Four pairs of chromosomes"), 
  
  
  list(
    image_filename = "male_female_quiz.jpg",
    question = "Is the male drosophila on the left or the right?",
    choices = c("The left", "The right"),
    correct = "The left"), 
  
  list(
    image_filename = "flybase_quiz.png",
    question = "Which online database is the primary resource for genetic information for Drosophila researchers?",
    choices = c("DrosophilaDB", "GeneCards", "FlyBase", "DrosophilaSearch"),
    correct = "FlyBase"), 
  
  list(
    image_filename = "gal4uas.png",
    question = "In Drosophila melanogaster research, what is the GAL4UAS system used for?",
    choices = c("Studying embryobnic development", "Analysing feeding behaviour", "Controlling gene expression in specific tissues or cells"),
    correct = "Controlling gene expression in specific tissues or cells"), 
  
  
  list(
    image_filename = "drosophila_similarity_quiz.PNG",
    question = "What genetic similarity does the Drosophila melanogaster genome have to humans?",
    choices = c("About 5%", "About 20%", "About 60%", "About 100%"),
    correct = "About 60%"), 
  
  list(
    image_filename = "drosophila_love.jpg",
    question = "What is the best insect in the universe?",
    choices = c("Drosophila melanogaster", "Drosophila melanogaster", "Drosophila melanogaster", "Drosophila melanogaster"),
    correct = "Drosophila melanogaster")
  
  
)

## Creating a function for the layout of the quiz questions
display_question <- function(question) {
  fluidRow(
    column(12,
           div(
             class = "d-flex justify-content-center mb-3",
             imageOutput("question_image")
           )
    ),
    column(12,
           h4(question$question, class = "text-left"),
           radioButtons("answer", "Pick one answer:", question$choices)
    )
  )
}

## creating actual ui and server for quiz
ui <- fluidPage(
  titlePanel("How well do you know Drosophila melanogaster?"),
  uiOutput("quiz_ui"),
  actionButton("submit_button", "Submit", class = "mt-3"),
)

server <- function(input, output, session) {
  current_question <- reactiveVal(1)
  score <- reactiveVal(0)
  
  observe({
    if (current_question() <= length(questions)) {
      question <- questions[[current_question()]]
      output$quiz_ui <- renderUI({
        display_question(question)
      })
      output$question_image <- renderImage({
        list(src = question$image_filename, width = 550, height = 400)
      }, deleteFile = FALSE)
    } else {
      output$quiz_ui <- renderText("Quiz completed. Your score:")
    }
  })
  
  observeEvent(input$submit_button, {
    if (current_question() <= length(questions)) {
      question <- questions[[current_question()]]
      if (input$answer == question$correct) {
        score(score() + 1)
      }
      current_question(current_question() + 1)
    }
  })
  
  observe({
    if (current_question() > length(questions)) {
      showModal(modalDialog(
        paste("Your score:", score(), "/", length(questions)),
        footer = NULL
      ))
    }
  })
}




## running the app
shinyApp(ui, server)

