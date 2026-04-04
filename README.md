
# Course Final Grade Calculator (Shiny App)

This Shiny app computes the final course grade from the following components:

- Pre-class Preparation (44 points)
- Class Participation (86 points)
- Homeworks: 13 homeworks, 15 points each (195 points total)
- Checkpoint Quizzes: 11 quizzes, 25 points each (275 points total)
- Mastery Exams: 2 exams, 200 points each (400 points total)

Total possible points: 1000

Run locally

- Install Shiny: `install.packages("shiny")`
- From R console: `shiny::runApp("app.R")` or open `app.R` in RStudio and click "Run App".

What the app does

- Provides numeric inputs for each assignment (each homework, quiz, and exam), plus pre-class and participation points.
- Calculates totals, shows a category breakdown, and displays the final points and percent.

Deploy to shinyapps.io

- Install rsconnect: `install.packages("rsconnect")`
- Configure account (see shinyapps.io docs) and then deploy: `rsconnect::deployApp()`

Files

- `app.R`: the Shiny application.

Letter grade cutoffs (out of 1000 points)

| Cutoff | Grade |
|--------:|:------|
| 930    | A     |
| 900    | A-    |
| 870    | B+    |
| 830    | B     |
| 800    | B-    |
| 770    | C+    |
| 730    | C     |
| 700    | C-    |
| 670    | D+    |
| 630    | D     |
| 600    | D-    |

Notes

- Scores below 600 are `F` by default in this scheme.

