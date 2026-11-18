# app.R - How Is AI Changing Jobs?  (Adem Khelifi and Sam)
# Click "Run App" to test. Commit and push when it works.
#
# PHOTOS: make a folder called "www" in the same folder as this file,
# and put your pictures in it named adem.jpg and sam.jpg

library(shiny)
library(dplyr)
library(ggplot2)

# ---- Data (same cleaning as analysis.Rmd) ----
data <- read.csv("ai_job_impact.csv", check.names = FALSE) %>%
  rename(Productivity_Change_Pct = `Productivity_Change_%`) %>%
  mutate(
    AI_Adoption_Level = factor(AI_Adoption_Level, levels = c("Low", "Medium", "High")),
    Job_Status        = factor(Job_Status, levels = c("Unchanged", "Modified", "Replaced")),
    Salary_Change_Pct = round((Salary_After_AI - Salary_Before_AI) / Salary_Before_AI * 100, 2)
  )

industries <- c("All", sort(unique(data$Industry)))

# ---- Simple styling for the homepage ----
home_style <- tags$style(HTML("
  .banner { background-color: #08519c; color: white; padding: 40px; border-radius: 10px; margin-bottom: 20px; }
  .box { background-color: #f5f8fc; padding: 20px; border-radius: 10px; margin-bottom: 20px; }
  .photo { width: 180px; height: 180px; border-radius: 50%; object-fit: cover; }
  .person { text-align: center; }
"))

# ---- UI: what people see ----
ui <- navbarPage(
  "How Is AI Changing Jobs?",
  header = home_style,

  tabPanel("Home",
    # Title banner
    div(class = "banner",
      h1("How Is AI Changing Jobs?"),
      h4("A look at how AI at work relates to salaries, productivity, and job security for 2,000 employees.")
    ),

    # The project
    div(class = "box",
      h3("The project"),
      p(strong("Our question: "), "How does AI adoption at work relate to salaries, productivity,",
        "and whether jobs are kept, changed, or replaced?"),
      p("We wanted to know whether working with more AI helps or hurts employees. We compare",
        "employees at workplaces with Low, Medium, and High AI adoption across different",
        "industries and job roles."),
      p(strong("How to use this app: "), "each tab at the top answers one question. Some tabs have",
        "dropdowns, buttons, or checkboxes so you can change what the chart shows.")
    ),

    # The dataset
    div(class = "box",
      h3("The dataset"),
      p("2,000 employees across 7 industries and 21 job roles. For each employee the data includes:"),
      tags$ul(
        tags$li(strong("Industry"), "and", strong("Job role")),
        tags$li(strong("AI adoption level"), "at their workplace: Low, Medium, or High"),
        tags$li(strong("Salary before and after AI"), "(we used these to calculate salary change %)"),
        tags$li(strong("Productivity change (%)")),
        tags$li(strong("Job satisfaction")),
        tags$li(strong("Job status:"), "Unchanged, Modified, or Replaced")
      ),
      p(strong("Source: "), "add link here")
    ),

    # About us
    h3("About us"),
    fluidRow(
      column(6, div(class = "box person",
        img(src = "adem.jpg", class = "photo", alt = "Photo of Adem"),
        h3("Adem Khelifi"),
        p(em("Mathematics major, Data Science and Computer Science minors")),
        p("Adem is a student at Washington and Lee University, originally from Tunisia.",
          "He is interested in a career as a data scientist. (Add more about yourself and",
          "what you worked on in this project.)")
      )),
      column(6, div(class = "box person",
        img(src = "sam.jpg", class = "photo", alt = "Photo of Sam"),
        h3("Sam"),
        p(em("Major and minor here")),
        p("Write Sam's bio here: where Sam is from, what Sam studies, career interests,",
          "and what Sam worked on in this project.")
      ))
    )
  ),

  tabPanel("Salary",
    h3("Salary change by AI adoption level"),
    selectInput("industry1", "Choose an industry:", choices = industries),
    checkboxInput("show_points1", "Show each employee as a dot", value = FALSE),
    plotOutput("graph1"),
    p(strong("Key finding:"), "salary change rises with AI adoption, from about +2.5% (Low) to +10.4% (High).")
  ),

  tabPanel("Before vs After",
    h3("Salary before vs after AI"),
    selectInput("industry2", "Choose an industry:", choices = industries),
    radioButtons("color2", "Color the dots by:",
                 choices = c("AI adoption level" = "AI_Adoption_Level", "Job status" = "Job_Status"),
                 inline = TRUE),
    plotOutput("graph2", hover = "hover2"),
    p(strong("Hover over a dot to see that employee:")),
    verbatimTextOutput("hover_info"),
    p("Points above the dashed line earned more after AI. About 67% of employees did.")
  ),

  tabPanel("Job Status",
    h3("Job status by industry"),
    radioButtons("bar_type3", "Show:", choices = c("Percent", "Count"), inline = TRUE),
    plotOutput("graph3"),
    p(strong("Key finding:"), "IT has the most jobs modified or replaced (about 51%); Manufacturing the fewest (about 38%).")
  ),

  tabPanel("Productivity",
    h3("Productivity change by AI adoption level"),
    selectInput("industry4", "Choose an industry:", choices = industries),
    plotOutput("graph4"),
    p(strong("Key finding:"), "productivity rose more with more AI: about 8% (Low) vs 11% (High).")
  ),

  tabPanel("Job Roles",
    h3("Job roles compared"),
    selectInput("measure5", "Choose what to compare:",
                choices = c("Average salary change (%)"       = "Salary_Change_Pct",
                            "Average productivity change (%)" = "Productivity_Change_Pct",
                            "Average job satisfaction"        = "Job_Satisfaction")),
    plotOutput("graph5", height = "550px"),
    p("For salary change: Doctors, Data Analysts, and Sales Associates gained the most (about +8%); Supervisors the least (about +4%).")
  ),

  tabPanel("Summary",
    h3("Summary table"),
    selectInput("group_by", "Group by:",
                choices = c("AI adoption level" = "AI_Adoption_Level",
                            "Industry"          = "Industry",
                            "Job status"        = "Job_Status")),
    tableOutput("summary_table")
  ),

  tabPanel("Data",
    h3("The data"),
    sliderInput("n_rows", "Number of rows to show:", min = 5, max = 100, value = 20, step = 5),
    downloadButton("download", "Download the full data (.csv)"),
    br(), br(),
    tableOutput("data_table")
  )
)

# ---- Server: what R makes ----
server <- function(input, output) {

  # keep only the chosen industry (or everything if "All")
  pick_industry <- function(choice) {
    if (choice == "All") data else filter(data, Industry == choice)
  }

  output$graph1 <- renderPlot({
    g <- ggplot(pick_industry(input$industry1),
                aes(x = AI_Adoption_Level, y = Salary_Change_Pct, fill = AI_Adoption_Level)) +
      geom_boxplot(show.legend = FALSE) +
      labs(x = "AI adoption level", y = "Salary change (%)") +
      theme_minimal(base_size = 14)
    if (input$show_points1) {
      g <- g + geom_jitter(width = 0.15, alpha = 0.3, show.legend = FALSE)
    }
    g
  })

  output$graph2 <- renderPlot({
    ggplot(pick_industry(input$industry2),
           aes(x = Salary_Before_AI, y = Salary_After_AI, color = .data[[input$color2]])) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(x = "Salary before AI ($)", y = "Salary after AI ($)", color = NULL) +
      theme_minimal(base_size = 14)
  })

  output$hover_info <- renderPrint({
    point <- nearPoints(pick_industry(input$industry2), input$hover2, maxpoints = 1)
    if (nrow(point) == 0) {
      cat("(no employee under the mouse)")
    } else {
      cat("Job role:      ", point$Job_Role, "\n")
      cat("Industry:      ", point$Industry, "\n")
      cat("AI adoption:   ", as.character(point$AI_Adoption_Level), "\n")
      cat("Job status:    ", as.character(point$Job_Status), "\n")
      cat("Salary before: $", point$Salary_Before_AI, "\n", sep = "")
      cat("Salary after:  $", point$Salary_After_AI, "\n", sep = "")
      cat("Change:        ", point$Salary_Change_Pct, "%\n", sep = "")
    }
  })

  output$graph3 <- renderPlot({
    bar_position <- if (input$bar_type3 == "Percent") "fill" else "stack"
    y_label      <- if (input$bar_type3 == "Percent") "Share of employees" else "Number of employees"
    ggplot(data, aes(x = Industry, fill = Job_Status)) +
      geom_bar(position = bar_position) +
      labs(x = NULL, y = y_label, fill = "Job status") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$graph4 <- renderPlot({
    ggplot(pick_industry(input$industry4),
           aes(x = AI_Adoption_Level, y = Productivity_Change_Pct)) +
      geom_boxplot(fill = "lightblue") +
      labs(x = "AI adoption level", y = "Productivity change (%)") +
      theme_minimal(base_size = 14)
  })

  output$graph5 <- renderPlot({
    data %>%
      group_by(Job_Role) %>%
      summarise(avg = mean(.data[[input$measure5]])) %>%
      ggplot(aes(x = reorder(Job_Role, avg), y = avg)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = NULL, y = "Average") +
      theme_minimal(base_size = 14)
  })

  output$summary_table <- renderTable({
    data %>%
      group_by(.data[[input$group_by]]) %>%
      summarise(
        Employees               = n(),
        `Avg salary change (%)` = mean(Salary_Change_Pct),
        `Avg productivity (%)`  = mean(Productivity_Change_Pct),
        `Avg satisfaction`      = mean(Job_Satisfaction)
      )
  })

  output$data_table <- renderTable({
    head(data, input$n_rows)
  })

  output$download <- downloadHandler(
    filename = "ai_job_impact_clean.csv",
    content  = function(file) write.csv(data, file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
