library(shiny)

# Letter grade cutoffs (out of 1000 total points)
GRADE_CUTOFFS <- data.frame(
  min_score = c(930, 900, 870, 830, 800, 770, 730, 700, 670, 630, 600, 0),
  grade = c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F"),
  stringsAsFactors = FALSE
)

MAX_QUIZ <- 100
MAX_HW <- 75
MAX_HOMEWORK <- MAX_HW * 2
MAX_EXAM_RAW <- 175
MAX_EXAM <- 250
MAX_PARTICIPATION <- 50
N_PARTICIPATION <- 14
PARTICIPATION_THRESHOLD <- 0.75
EVAL_EXTRA_CREDIT <- 10

numeric_to_letter <- function(total_score) {
  idx <- which(total_score >= GRADE_CUTOFFS$min_score)[1]
  GRADE_CUTOFFS$grade[idx]
}

participation_points <- function(completed) {
  pct <- completed / N_PARTICIPATION
  if (pct >= PARTICIPATION_THRESHOLD) {
    MAX_PARTICIPATION
  } else {
    pct * MAX_PARTICIPATION
  }
}

reweight_exam <- function(raw_score) {
  raw_score * MAX_EXAM / MAX_EXAM_RAW
}

ui <- fluidPage(
  titlePanel("STA235 Grade Calculator"),
  sidebarLayout(
    sidebarPanel(
      h4("Quizzes (100 pts each)"),
      numericInput("quiz1", "Quiz 1", value = 0, min = 0, max = MAX_QUIZ, step = 0.5),
      numericInput("quiz2", "Quiz 2", value = 0, min = 0, max = MAX_QUIZ, step = 0.5),
      numericInput("quiz3", "Quiz 3", value = 0, min = 0, max = MAX_QUIZ, step = 0.5),
      hr(),
      h4("Homework (75 pts each, 150 total)"),
      numericInput("hw1", "Homework 1", value = 0, min = 0, max = MAX_HW, step = 0.5),
      numericInput("hw2", "Homework 2", value = 0, min = 0, max = MAX_HW, step = 0.5),
      hr(),
      h4("Exams (enter score out of 175; reweighted to 250 pts each)"),
      numericInput("exam1", "Exam 1 (Unit A)", value = 0, min = 0, max = MAX_EXAM_RAW, step = 0.5),
      numericInput("exam2", "Exam 2 (Unit B)", value = 0, min = 0, max = MAX_EXAM_RAW, step = 0.5),
      hr(),
      h4("Participation"),
      sliderInput(
        "participation_done",
        paste0("Assignments completed (out of ", N_PARTICIPATION, ")"),
        min = 0,
        max = N_PARTICIPATION,
        value = 0,
        step = 1
      ),
      helpText(
        "Full 50 pts at >= 75% completion (11+ assignments). ",
        "Below 75%, points = (completed / 14) x 50."
      ),
      hr(),
      h4("Extra Credit"),
      checkboxInput(
        "eval_completed",
        paste0("Completed professor's evaluation (+", EVAL_EXTRA_CREDIT, " pts)"),
        value = FALSE
      )
    ),
    mainPanel(
      h3("Your Grade"),
      verbatimTextOutput("summary"),
      hr(),
      h4("Grade Scale (out of 1000)"),
      tableOutput("grade_table")
    )
  )
)

server <- function(input, output, session) {
  grades <- reactive({
    quiz_pts <- input$quiz1 + input$quiz2 + input$quiz3
    hw1_pts <- input$hw1
    hw2_pts <- input$hw2
    hw_pts <- hw1_pts + hw2_pts
    exam1_pts <- reweight_exam(input$exam1)
    exam2_pts <- reweight_exam(input$exam2)
    exam_pts <- exam1_pts + exam2_pts
    part_pct <- input$participation_done / N_PARTICIPATION
    part_pts <- participation_points(input$participation_done)
    ec_pts <- if (isTRUE(input$eval_completed)) EVAL_EXTRA_CREDIT else 0
    total <- quiz_pts + hw_pts + exam_pts + part_pts + ec_pts
    letter <- numeric_to_letter(total)

    list(
      quiz_pts = quiz_pts,
      hw1_pts = hw1_pts,
      hw2_pts = hw2_pts,
      hw_pts = hw_pts,
      exam1_raw = input$exam1,
      exam2_raw = input$exam2,
      exam1_pts = exam1_pts,
      exam2_pts = exam2_pts,
      exam_pts = exam_pts,
      part_pct = part_pct,
      part_pts = part_pts,
      ec_pts = ec_pts,
      total = total,
      letter = letter
    )
  })

  output$summary <- renderText({
    g <- grades()
    paste0(
      "Quizzes:        ", sprintf("%6.1f", g$quiz_pts), " / 300\n",
      "Homework 1:     ", sprintf("%6.1f", g$hw1_pts), " / 75\n",
      "Homework 2:     ", sprintf("%6.1f", g$hw2_pts), " / 75\n",
      "Homework total: ", sprintf("%6.1f", g$hw_pts), " / 150\n",
      "Exam 1 (Unit A): ", sprintf("%6.1f", g$exam1_pts), " / 250",
      "  (raw ", sprintf("%.1f", g$exam1_raw), " / ", MAX_EXAM_RAW, ")\n",
      "Exam 2 (Unit B): ", sprintf("%6.1f", g$exam2_pts), " / 250",
      "  (raw ", sprintf("%.1f", g$exam2_raw), " / ", MAX_EXAM_RAW, ")\n",
      "Exams total:    ", sprintf("%6.1f", g$exam_pts), " / 500\n",
      "Participation:  ", sprintf("%6.1f", g$part_pts), " / 50",
      "  (", sprintf("%.0f%%", g$part_pct * 100), " completed)\n",
      "Extra credit:   ", sprintf("%6.1f", g$ec_pts), " / ", EVAL_EXTRA_CREDIT,
      "  (professor's evaluation)\n",
      "--------------------------------\n",
      "Total score:    ", sprintf("%6.1f", g$total), " / 1000",
      if (g$ec_pts > 0) paste0(" (+ ", g$ec_pts, " EC)") else "", "\n",
      "Letter grade:   ", g$letter, "\n"
    )
  })

  output$grade_table <- renderTable({
    data.frame(
      Cutoff = GRADE_CUTOFFS$min_score[GRADE_CUTOFFS$grade != "F"],
      Grade = GRADE_CUTOFFS$grade[GRADE_CUTOFFS$grade != "F"],
      check.names = FALSE
    )
  }, bordered = TRUE, striped = TRUE)
}

shinyApp(ui = ui, server = server)
