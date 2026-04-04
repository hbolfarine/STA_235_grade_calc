library(shiny)

# Grade calculator Shiny app for the course

MAX <- list(
  pre = 44,
  participation = 86,
  hw = 15,    # per homework
  hw_count = 13,
  quiz = 25,  # per quiz
  quiz_count = 11,
  exam = 200, # per mastery exam
  exam_count = 2
)

total_points <- function() {
  MAX$pre + MAX$participation + (MAX$hw * MAX$hw_count) + (MAX$quiz * MAX$quiz_count) + (MAX$exam * MAX$exam_count)
}

# Letter grade cutoffs (out of 1000 total points)
GRADE_CUTOFFS <- c(
  "A" = 930,
  "A-" = 900,
  "B+" = 870,
  "B" = 830,
  "B-" = 800,
  "C+" = 770,
  "C" = 730,
  "C-" = 700,
  "D+" = 670,
  "D" = 630,
  "D-" = 600
)

letter_grade <- function(points) {
  ordered <- GRADE_CUTOFFS[order(-GRADE_CUTOFFS)]
  idx <- which(points >= ordered)
  if (length(idx) == 0) return("F")
  names(ordered)[idx[1]]
}

clamp <- function(x, min = 0, max = Inf) {
  x <- as.numeric(x)
  if (is.na(x)) return(0)
  pmin(pmax(x, min), max)
}

ui <- fluidPage(
  titlePanel("Course Final Grade Calculator"),
  sidebarLayout(
    sidebarPanel(width = 4,
      numericInput("pre", "Pre-class Preparation (Perusall) (0-44):", value = 0, min = 0, max = MAX$pre, step = 1),
      numericInput("part", "Class Participation (0-86):", value = 0, min = 0, max = MAX$participation, step = 1),
      tags$hr(),
      h4("Homeworks (15 pts each, 13 total)"),
      wellPanel(
        do.call(tagList, lapply(1:MAX$hw_count, function(i) {
          numericInput(paste0("hw", i), paste0("HW ", i, " (0-", MAX$hw, ")"), value = 0, min = 0, max = MAX$hw, step = 1)
        }))
      ),
      tags$hr(),
      h4("Checkpoint Quizzes (25 pts each, 11 total)"),
      wellPanel(
        do.call(tagList, lapply(1:MAX$quiz_count, function(i) {
          numericInput(paste0("q", i), paste0("Quiz ", i, " (0-", MAX$quiz, ")"), value = 0, min = 0, max = MAX$quiz, step = 1)
        }))
      ),
      tags$hr(),
      h4("Mastery Exams (200 pts each, 2 total)"),
      wellPanel(
        do.call(tagList, lapply(1:MAX$exam_count, function(i) {
          numericInput(paste0("exam", i), paste0("Exam ", i, " (0-", MAX$exam, ")"), value = 0, min = 0, max = MAX$exam, step = 1)
        }))
      ),
      actionButton("reset", "Reset all to 0")
    ),

    mainPanel(
      h3("Final Grade Summary"),
      verbatimTextOutput("total_text"),
      tags$hr(),
      h4("Breakdown"),
      tableOutput("breakdown"),
      tags$hr(),
      p("Total possible points: ", total_points())
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateNumericInput(session, "pre", value = 0)
    updateNumericInput(session, "part", value = 0)
    for (i in 1:MAX$hw_count) updateNumericInput(session, paste0("hw", i), value = 0)
    for (i in 1:MAX$quiz_count) updateNumericInput(session, paste0("q", i), value = 0)
    for (i in 1:MAX$exam_count) updateNumericInput(session, paste0("exam", i), value = 0)
  })

  calc <- reactive({
    pre <- clamp(input$pre, 0, MAX$pre)
    part <- clamp(input$part, 0, MAX$participation)

    hws <- sapply(1:MAX$hw_count, function(i) clamp(input[[paste0('hw', i)]], 0, MAX$hw))
    quizzes <- sapply(1:MAX$quiz_count, function(i) clamp(input[[paste0('q', i)]], 0, MAX$quiz))
    exams <- sapply(1:MAX$exam_count, function(i) clamp(input[[paste0('exam', i)]], 0, MAX$exam))

    hw_total <- sum(hws)
    quiz_total <- sum(quizzes)
    exam_total <- sum(exams)

    total <- pre + part + hw_total + quiz_total + exam_total
    pct <- (total / total_points()) * 100

    list(pre = pre, part = part, hw_total = hw_total, hw_count = MAX$hw_count,
         quiz_total = quiz_total, quiz_count = MAX$quiz_count,
         exam_total = exam_total, exam_count = MAX$exam_count,
         total = total, pct = pct)
  })

  output$total_text <- renderText({
    v <- calc()
    lg <- letter_grade(v$total)
    paste0("Total points earned: ", v$total, " / ", total_points(), " (", sprintf("%.2f", v$pct), "%) - Letter grade: ", lg)
  })

  output$breakdown <- renderTable({
    v <- calc()
    data.frame(
      Category = c("Pre-class Preparation", "Class Participation", paste0("Homeworks (", v$hw_count, ")"), paste0("Quizzes (", v$quiz_count, ")"), paste0("Mastery Exams (", v$exam_count, ")")),
      Earned = c(v$pre, v$part, v$hw_total, v$quiz_total, v$exam_total),
      Possible = c(MAX$pre, MAX$participation, MAX$hw * MAX$hw_count, MAX$quiz * MAX$quiz_count, MAX$exam * MAX$exam_count),
      stringsAsFactors = FALSE
    )
  }, digits = 2)
}

shinyApp(ui, server)
