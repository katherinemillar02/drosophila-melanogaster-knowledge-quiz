### PACKAGES----####

library(shiny)



## Setting the quiz questions
quiz_questions <- list(
  list(
    image_url = "images/drosophila_melanogaster_quiz.jpg",
    question = "What is the common name for Drosophila melanogaster?",
    choices = c("House Fly", "Fruit Fly", "Mosquito", "Butterfly"),
    correct = 1
  ),
  list(
    image_url = "images/sex_chromosomes_quiz.png",
    question = "Which chromosome determines the sex of Drosophila melanogaster?",
    choices = c("X chromosome", "Y chromosome", "Both X and Y chromosomes"),
    correct = 1
  )
)


## Creating a function for the layout of the quiz questions
question_layout <- function(quiz_questions) {
  tagList(
    tags$img(src = question$image_url, width = 200, height = 150),
    h4(question$question),
    radioButtons("answer", "Pick one answer:", question$choices)
  )
}

