# app.R — How Is AI Changing Jobs?  (Adem Khelifi and Sam)
# Click "Run App" to test. Commit and push when it works.

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

# ---- UI: what people see ----
ui <- navbarPage(
  "How Is AI Changing Jobs?",

  tabPanel("About",
    h3("Our question"),
    p("How does AI adoption at work relate to salaries, productivity, and whether jobs are kept, changed, or replaced?"),
    h3("Data"),
    p("2,000 employees across 7 industries and 21 job roles. Source: add link here."),
    h3("How to use this app"),
    p("Each tab answers one question. Some tabs have a dropdown to look at one industry at a time."),
    h3("Team"),
    p("Adem Khelifi and Sam")
  ),

  tabPanel("Salary",
    h3("Salary change by AI adoption level"),
    selectInput("industry1", "Choose an industry:", choices = industries),
    plotOutput("graph1"),
    p(strong("Key finding:"), "salary change rises with AI adoption, from about +2.5% (Low) to +10.4% (High).")
  ),

  tabPanel("Before vs After",
    h3("Salary before vs after AI"),
    selectInput("industry2", "Choose an industry:", choices = industries),
    plotOutput("graph2"),
    p("Points above the dashed line earned more after AI. About 67% of employees did.")
  ),

  tabPanel("Job Status",
    h3("Job status by industry"),
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
    h3("Average salary change by job role"),
    plotOutput("graph5", height = "550px"),
    p("Doctors, Data Analysts, and Sales Associates gained the most (about +8%); Supervisors the least (about +4%).")
  ),

  tabPanel("Summary",
    h3("Summary by AI adoption level"),
    tableOutput("summary_table")
  ),

  tabPanel("Data",
    h3("First 20 rows"),
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
    ggplot(pick_industry(input$industry1),
           aes(x = AI_Adoption_Level, y = Salary_Change_Pct, fill = AI_Adoption_Level)) +
      geom_boxplot(show.legend = FALSE) +
      labs(x = "AI adoption level", y = "Salary change (%)") +
      theme_minimal(base_size = 14)
  })

  output$graph2 <- renderPlot({
    ggplot(pick_industry(input$industry2),
           aes(x = Salary_Before_AI, y = Salary_After_AI, color = AI_Adoption_Level)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(x = "Salary before AI ($)", y = "Salary after AI ($)", color = "AI adoption") +
      theme_minimal(base_size = 14)
  })

  output$graph3 <- renderPlot({
    ggplot(data, aes(x = Industry, fill = Job_Status)) +
      geom_bar(position = "fill") +
      labs(x = NULL, y = "Share of employees", fill = "Job status") +
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
      summarise(avg_change = mean(Salary_Change_Pct)) %>%
      ggplot(aes(x = reorder(Job_Role, avg_change), y = avg_change)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = NULL, y = "Average salary change (%)") +
      theme_minimal(base_size = 14)
  })

  output$summary_table <- renderTable({
    data %>%
      group_by(AI_Adoption_Level) %>%
      summarise(
        Employees              = n(),
        `Avg salary change (%)` = mean(Salary_Change_Pct),
        `Avg productivity (%)`  = mean(Productivity_Change_Pct),
        `Avg satisfaction`      = mean(Job_Satisfaction)
      )
  })

  output$data_table <- renderTable({
    head(data, 20)
  })
}

shinyApp(ui = ui, server = server)
