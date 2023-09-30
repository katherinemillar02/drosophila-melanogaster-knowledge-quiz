library(shiny)

## Setting the quiz questions
questions <- list(
  list(
    image_filename = "drosophila_melanogaster_quiz.jpg",
    question = "What is the common name for Drosophila melanogaster?",
    choices = c("House Fly", "Fruit Fly", "Mosquito", "Butterfly"),
    correct = "Fruit Fly"
  ),
  list(
    image_filename = "sex_chromosomes_quiz.png",
    question = "Which chromosome determines the sex of Drosophila melanogaster?",
    choices = c("X chromosome", "Y chromosome", "Both X and Y chromosomes"),
    correct = "X chromosome"
  )
)

## Creating a function for the layout of the quiz questions
display_question <- function(question) {
  tagList(
    imageOutput("question_image"),
    h4(question$question),
    radioButtons("answer", "Pick one answer:", question$choices)
  )
}

## creating actual ui and server for quiz
ui <- fluidPage(
  titlePanel("How well do you know Drosophila melanogaster?"),
  imageOutput("quiz_image"),
  uiOutput("quiz_ui"),
  actionButton("submit_button", "Submit"),
 
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
      output$quiz_image <- renderImage({
        list(src = paste0(question$image_filename), width = 200, height = 150)
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

