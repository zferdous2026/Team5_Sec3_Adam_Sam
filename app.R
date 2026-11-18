# app.R - How Is AI Changing Jobs?  (Adem Khelifi and Sam)  -- QUESTIONNAIRE VERSION
# Click "Run App" to test. Commit and push when it works.
# Uses only shiny, dplyr and ggplot2 (no new packages needed).
#
# PHOTOS: make a folder called "www" next to this file and put the two photos in it:
# AdemKHeadshot2026.jpg and Tignor.jpeg (names must match exactly, including capitals).
# Until the photos are there, the app shows a circle with your initials.

library(shiny)
library(dplyr)
library(ggplot2)

# =====================================================================
# ---- EDIT ME: team bios and project text ----
# =====================================================================
team <- list(
  list(name  = "Adem Khelifi",
       photo = "AdemKHeadshot2026.jpg",
       role  = "Mathematics major, Data Science and Computer Science minors",
       bio   = paste("I'm Adem, a student at Washington and Lee University, originally from Tunisia.",
                     "I'm interested in a career as a data scientist.")),
  list(name  = "Sam Tignor",
       photo = "Tignor.jpeg",
       role  = "Biology major",
       bio   = paste("I'm Sam Tignor, a pre-med student at Washington and Lee University pursuing a",
                     "B.S. in Biology and graduating in 2028. I'm on the wrestling team, and I hope to",
                     "go to medical school and become a family medicine physician."))
)

data_source <- "add link here"   # e.g. "https://www.kaggle.com/..."

# =====================================================================
# ---- Data (same cleaning as analysis.Rmd) ----
# =====================================================================
data <- read.csv("ai_job_impact.csv", check.names = FALSE) %>%
  rename(Productivity_Change_Pct = `Productivity_Change_%`) %>%
  mutate(
    AI_Adoption_Level = factor(AI_Adoption_Level, levels = c("Low", "Medium", "High")),
    Job_Status        = factor(Job_Status, levels = c("Unchanged", "Modified", "Replaced")),
    Salary_Change_Pct = round((Salary_After_AI - Salary_Before_AI) / Salary_Before_AI * 100, 2)
  )

industries <- sort(unique(data$Industry))
sal_range  <- c(floor(min(data$Salary_Before_AI) / 1000) * 1000,
                ceiling(max(data$Salary_Before_AI) / 1000) * 1000)

# ---- Shared look ----
level_colors  <- c(Low = "#9ecae1", Medium = "#4292c6", High = "#08519c")
status_colors <- c(Unchanged = "#bdbdbd", Modified = "#fd8d3c", Replaced = "#d7301f")
you_color     <- "#fd8d3c"
other_color   <- "#9ecae1"
my_theme      <- theme_minimal(base_size = 14)

fmt_pct   <- function(x) sprintf("%+.1f%%", x)
fmt_share <- function(x) sprintf("%.0f%%", x)

# What each "what matters most" answer means in the data
priorities <- list(
  salary       = list(var = "Salary_Change_Pct",       label = "Average salary change (%)",       higher_is_better = TRUE),
  security     = list(var = "Replaced_Pct",            label = "Share of jobs replaced (%)",      higher_is_better = FALSE),
  productivity = list(var = "Productivity_Change_Pct", label = "Average productivity change (%)", higher_is_better = TRUE),
  satisfaction = list(var = "Job_Satisfaction",        label = "Average job satisfaction",        higher_is_better = TRUE)
)
priority_names <- c("Salary growth" = "salary", "Job security" = "security",
                    "Becoming more productive" = "productivity", "Job satisfaction" = "satisfaction")

# ---- Helper functions ----
role_table <- function(d) {
  d %>% group_by(Job_Role) %>%
    summarise(Employees               = n(),
              Salary_Change_Pct       = mean(Salary_Change_Pct),
              Productivity_Change_Pct = mean(Productivity_Change_Pct),
              Job_Satisfaction        = mean(Job_Satisfaction),
              Replaced_Pct            = mean(Job_Status == "Replaced") * 100,
              Unchanged_Pct           = mean(Job_Status == "Unchanged") * 100,
              .groups = "drop")
}

# box plot by AI level, with the student's expected level in orange
level_box <- function(d, yvar, ylab, ai_choice) {
  d$hl <- if (ai_choice == "Not sure") "other" else ifelse(d$AI_Adoption_Level == ai_choice, "you", "other")
  ggplot(d, aes(x = AI_Adoption_Level, y = .data[[yvar]], fill = hl)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    scale_fill_manual(values = c(you = you_color, other = other_color)) +
    scale_x_discrete(drop = FALSE) +
    labs(x = "AI adoption level", y = ylab, caption = "White diamond = average") +
    my_theme
}

stat_card <- function(value, label, compare = NULL) {
  div(class = "stat-card",
      div(class = "stat-value", value),
      div(class = "stat-label", label),
      if (!is.null(compare)) div(class = "stat-compare", compare))
}

person_card <- function(person) {
  photo <- if (file.exists(file.path("www", person$photo))) {
    tags$img(src = person$photo, class = "team-photo", alt = person$name)
  } else {
    initials <- paste(substr(strsplit(person$name, " ")[[1]], 1, 1), collapse = "")
    div(class = "team-photo placeholder", initials)
  }
  div(class = "card-box team-card",
      photo,
      h3(person$name),
      p(class = "team-role", person$role),
      p(person$bio))
}

# =====================================================================
# ---- Styling ----
# =====================================================================
css <- "
body { background: #fafbfc; }
.hero { background: linear-gradient(135deg, #08519c, #4292c6); color: white;
        padding: 55px 40px; border-radius: 14px; margin: 10px 0 30px 0; }
.hero h1 { font-weight: 700; font-size: 42px; margin-top: 0; }
.hero p.lead { font-size: 20px; max-width: 760px; }
.hero .btn { margin-top: 15px; font-size: 18px; padding: 10px 26px; border-radius: 30px;
             background: white; color: #08519c; border: none; font-weight: 600; }
.section-title { font-weight: 700; color: #08519c; margin: 35px 0 15px 0; }
.card-box { background: white; border: 1px solid #e6e9ee; border-radius: 12px; padding: 22px;
            box-shadow: 0 1px 4px rgba(0,0,0,0.05); margin-bottom: 20px; }
.team-card { text-align: center; min-height: 420px; }
.team-photo { width: 170px; height: 170px; border-radius: 50%; object-fit: cover;
              display: block; margin: 0 auto 15px auto; border: 4px solid #deebf7; }
.placeholder { background: #4292c6; color: white; font-size: 54px; line-height: 162px; text-align: center; }
.team-role { color: #6b7280; font-style: italic; }
.stat-card { background: white; border: 1px solid #e6e9ee; border-radius: 12px; padding: 18px;
             text-align: center; margin-bottom: 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }
.stat-value { font-size: 32px; font-weight: 700; color: #08519c; }
.stat-label { font-size: 15px; color: #374151; }
.stat-compare { font-size: 13px; color: #6b7280; margin-top: 4px; }
.step-num { display: inline-block; width: 36px; height: 36px; border-radius: 50%; background: #08519c;
            color: white; text-align: center; line-height: 36px; font-weight: 700; margin-right: 8px; }
.profile-banner { background: #eef5fc; border-left: 6px solid #4292c6; padding: 14px 20px;
                  border-radius: 8px; margin-bottom: 22px; font-size: 15px; }
.finding { background: #fff8f0; border-left: 5px solid #fd8d3c; padding: 12px 18px;
           border-radius: 6px; margin: 10px 0 25px 0; font-size: 15px; }
.quiz .form-group { margin-bottom: 22px; }
.quiz label { font-size: 16px; }
"

# =====================================================================
# ---- UI ----
# =====================================================================
results_tab <- function(title, value, ...) {
  tabPanel(title, value = value, uiOutput(paste0("banner_", value)), ...)
}

ui <- navbarPage(
  "How Is AI Changing Jobs?",
  id = "nav",
  collapsible = TRUE,
  header = tags$head(tags$style(HTML(css))),
  
  # ---------------- 1. HOME ----------------
  tabPanel("Home", value = "home",
           div(class = "hero",
               h1("How Is AI Changing Jobs?"),
               p(class = "lead",
                 "Thinking about your future career? See how AI at work has affected salaries,",
                 "productivity, and job security for 2,000 employees, and find out what it could mean for the career you want."),
               actionButton("go_quiz", "Start: tell us about your plans  \u2192")
           ),
           
           h2(class = "section-title", "About the project"),
           fluidRow(
             column(7, div(class = "card-box",
                           h4("Our question"),
                           p("How does AI adoption at work relate to salaries, productivity, and whether jobs are kept,",
                             "changed, or replaced?"),
                           h4("Why it matters for students"),
                           p("Students are choosing majors and careers while AI is changing the workplace. Instead of",
                             "general headlines, this app lets you look at the data for the specific industry and job you",
                             "are interested in."),
                           h4("What we found overall"),
                           uiOutput("home_findings")
             )),
             column(5, div(class = "card-box",
                           h4("How it works"),
                           p(span(class = "step-num", "1"), "Answer a few questions about the career you want."),
                           p(span(class = "step-num", "2"), "We pull out the employees in the data who match your plans."),
                           p(span(class = "step-num", "3"), "Explore your results: salary, job security, productivity,",
                             "and how your career compares with others."),
                           br(),
                           actionButton("go_quiz2", "Go to the questionnaire", class = "btn-primary")
             ))
           ),
           
           h2(class = "section-title", "The dataset"),
           uiOutput("home_stats"),
           fluidRow(
             column(5, div(class = "card-box",
                           h4("Where it comes from"),
                           p("Each row is one employee. The data records their industry and job role, how much AI their",
                             "workplace uses, their salary before and after AI was introduced, how their productivity",
                             "changed, how satisfied they are, and whether their job stayed the same, was modified, or was replaced."),
                           p(strong("Source: "), data_source),
                           p(em("Note: we calculated salary change (%) ourselves from the before and after salaries."))
             )),
             column(7, div(class = "card-box",
                           h4("Variables we use"),
                           tableOutput("variables_table")
             ))
           ),
           
           h2(class = "section-title", "About us"),
           fluidRow(
             column(6, person_card(team[[1]])),
             column(6, person_card(team[[2]]))
           )
  ),
  
  # ---------------- 2. QUESTIONNAIRE ----------------
  tabPanel("Your Plans", value = "plans",
           fluidRow(column(8, offset = 2,
                           div(class = "card-box quiz",
                               h2("Tell us about your plans"),
                               p("Answer these questions and the rest of the app will show results for the career you picked.",
                                 "You can come back and change your answers any time."),
                               hr(),
                               fluidRow(
                                 column(6, textInput("q_name", "What's your first name?", placeholder = "e.g. Jordan")),
                                 column(6, selectInput("q_year", "What year are you?",
                                                       choices = c("First-year", "Sophomore", "Junior", "Senior", "Graduate student", "Other")))
                               ),
                               textInput("q_major", "What are you studying (major or intended major)?", placeholder = "e.g. Economics"),
                               selectInput("q_industry", "Which industry do you most want to work in?", choices = industries),
                               selectInput("q_role", "Which job are you aiming for?", choices = NULL),
                               radioButtons("q_ai", "How much AI do you expect to use in that job?",
                                            choices = c("Low", "Medium", "High", "Not sure"), selected = "Not sure", inline = TRUE),
                               radioButtons("q_feel", "How do you feel about AI in the workplace?",
                                            choices = c("Excited", "Neutral", "Worried"), selected = "Neutral", inline = TRUE),
                               radioButtons("q_priority", "What matters most to you in a career?", choices = priority_names),
                               sliderInput("q_salary", "What salary are you hoping for?", min = sal_range[1], max = sal_range[2],
                                           value = round(mean(data$Salary_Before_AI), -3), step = 1000, pre = "$", sep = ",",
                                           width = "100%"),
                               br(),
                               actionButton("submit", "Show my results  \u2192", class = "btn-primary btn-lg")
                           )
           ))
  ),
  
  # ---------------- 3+. RESULTS (based on the answers) ----------------
  results_tab("Your Outlook", "outlook",
              uiOutput("outlook_cards"),
              uiOutput("outlook_text"),
              h3("How often are jobs changed or replaced?"),
              plotOutput("outlook_plot", height = "260px")
  ),
  
  results_tab("Salary", "salary",
              h3("How did salaries change in your role?"),
              plotOutput("salary_box"),
              uiOutput("salary_finding1"),
              h3("People in your role who earn about what you hope to earn"),
              fluidRow(
                column(9, plotOutput("salary_scatter", hover = hoverOpts("salary_hover", delay = 100, delayType = "debounce"))),
                column(3, div(class = "card-box", uiOutput("salary_hover_info")))
              ),
              uiOutput("salary_finding2")
  ),
  
  results_tab("Job Security", "security",
              h3("Does more AI mean more jobs replaced in your role?"),
              plotOutput("security_by_level", height = "280px"),
              uiOutput("security_finding1"),
              h3("Which jobs in your industry are replaced most often?"),
              plotOutput("security_roles", height = "520px"),
              uiOutput("security_finding2")
  ),
  
  results_tab("Productivity & Satisfaction", "prodsat",
              fluidRow(
                column(6, h3("Productivity change"), plotOutput("prod_box")),
                column(6, h3("Job satisfaction"),    plotOutput("sat_box"))
              ),
              uiOutput("prodsat_finding")
  ),
  
  results_tab("Compare Careers", "compare",
              fluidRow(
                column(4, radioButtons("cmp_scope", "Compare against",
                                       choices = c("Jobs in my industry" = "industry", "All jobs" = "all"), inline = TRUE)),
                column(4, selectInput("cmp_metric", "Rank jobs by", choices = priority_names))
              ),
              uiOutput("compare_finding"),
              plotOutput("compare_plot", height = "560px"),
              h3("All the numbers"),
              tableOutput("compare_table")
  ),
  
  results_tab("Data", "data",
              fluidRow(
                column(5, radioButtons("data_scope", "Show",
                                       choices = c("My job" = "role", "My job in my industry" = "match",
                                                   "My industry" = "industry", "Everyone" = "all"), inline = TRUE)),
                column(3, numericInput("n_rows", "Rows to show", value = 20, min = 5, max = 500, step = 5)),
                column(4, br(), downloadButton("download", "Download these rows (.csv)"))
              ),
              tableOutput("data_table")
  )
)

# =====================================================================
# ---- Server ----
# =====================================================================
server <- function(input, output, session) {
  
  # ---------------- Home ----------------
  observeEvent(input$go_quiz,  updateNavbarPage(session, "nav", selected = "plans"))
  observeEvent(input$go_quiz2, updateNavbarPage(session, "nav", selected = "plans"))
  
  output$home_stats <- renderUI({
    fluidRow(
      column(3, stat_card(format(nrow(data), big.mark = ","), "employees")),
      column(3, stat_card(length(unique(data$Industry)), "industries")),
      column(3, stat_card(length(unique(data$Job_Role)), "job roles")),
      column(3, stat_card(3, "levels of AI adoption", "Low \u00b7 Medium \u00b7 High"))
    )
  })
  
  output$home_findings <- renderUI({
    by_level <- data %>% group_by(AI_Adoption_Level) %>%
      summarise(s = mean(Salary_Change_Pct), pr = mean(Productivity_Change_Pct), .groups = "drop")
    ind <- data %>% group_by(Industry) %>%
      summarise(ch = mean(Job_Status != "Unchanged") * 100, .groups = "drop") %>% arrange(desc(ch))
    tags$ul(
      tags$li(sprintf("Salary change rises with AI adoption: %s (Low) to %s (High).",
                      fmt_pct(by_level$s[1]), fmt_pct(by_level$s[3]))),
      tags$li(sprintf("Productivity rose more with more AI: %s (Low) vs %s (High).",
                      fmt_pct(by_level$pr[1]), fmt_pct(by_level$pr[3]))),
      tags$li(sprintf("%.0f%% of employees earned more after AI.",
                      mean(data$Salary_After_AI > data$Salary_Before_AI) * 100)),
      tags$li(sprintf("%s has the most jobs modified or replaced (%.0f%%); %s the fewest (%.0f%%).",
                      ind$Industry[1], ind$ch[1], ind$Industry[nrow(ind)], ind$ch[nrow(ind)]))
    )
  })
  
  output$variables_table <- renderTable({
    data.frame(
      Variable = c("Industry", "Job role", "AI adoption level", "Salary before / after AI",
                   "Salary change (%)", "Productivity change (%)", "Job satisfaction", "Job status"),
      Meaning  = c(paste(length(unique(data$Industry)), "industries"),
                   paste(length(unique(data$Job_Role)), "different jobs"),
                   "How much the workplace uses AI: Low, Medium, High",
                   "Yearly salary in dollars",
                   "How much salary went up or down (we calculated this)",
                   "How much the employee's productivity changed",
                   sprintf("Satisfaction score (from %g to %g in this data)",
                           min(data$Job_Satisfaction), max(data$Job_Satisfaction)),
                   "Unchanged, Modified, or Replaced")
    )
  })
  
  # ---------------- Questionnaire ----------------
  observeEvent(input$q_industry, {
    r <- sort(unique(data$Job_Role[data$Industry == input$q_industry]))
    updateSelectInput(session, "q_role", choices = r,
                      selected = if (isTRUE(input$q_role %in% r)) input$q_role else r[1])
  })
  
  answers <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    req(input$q_role)
    nm <- trimws(input$q_name)
    answers(list(
      name     = if (nm == "") "you" else nm,
      year     = input$q_year,
      major    = trimws(input$q_major),
      industry = input$q_industry,
      role     = input$q_role,
      ai       = input$q_ai,
      feel     = input$q_feel,
      priority = input$q_priority,
      salary   = input$q_salary
    ))
    updateSelectInput(session, "cmp_metric", selected = input$q_priority)
    showNotification("Your results are ready!", type = "message", duration = 3)
    updateNavbarPage(session, "nav", selected = "outlook")
  })
  
  # every results output starts with this: stop if the questionnaire isn't done
  get_answers <- function() {
    a <- answers()
    validate(need(!is.null(a), "Fill in the 'Your Plans' page first, then click 'Show my results'."))
    a
  }
  
  role_d  <- reactive({ a <- get_answers(); filter(data, Job_Role == a$role) })
  ind_d   <- reactive({ a <- get_answers(); filter(data, Industry == a$industry) })
  match_d <- reactive({ a <- get_answers(); filter(data, Job_Role == a$role, Industry == a$industry) })
  
  # banner at the top of every results page, with a link back to the questionnaire
  result_pages <- c("outlook", "salary", "security", "prodsat", "compare", "data")
  for (pg in result_pages) {
    local({
      page <- pg
      output[[paste0("banner_", page)]] <- renderUI({
        a <- answers()
        if (is.null(a)) {
          return(div(class = "profile-banner",
                     "These pages show results based on your answers. ",
                     actionLink(paste0("edit_", page), "Answer the questionnaire first \u2192")))
        }
        who <- if (a$name == "you") "Your results" else paste0(a$name, "'s results")
        div(class = "profile-banner",
            strong(who), " \u00b7 Aiming for: ", strong(a$role), " in ", strong(a$industry),
            " \u00b7 Expected AI use: ", a$ai,
            " \u00b7 Cares most about: ", names(priority_names)[priority_names == a$priority],
            " \u00b7 ", actionLink(paste0("edit_", page), "Change my answers"))
      })
      observeEvent(input[[paste0("edit_", page)]], updateNavbarPage(session, "nav", selected = "plans"))
    })
  }
  
  # ---------------- Your Outlook ----------------
  output$outlook_cards <- renderUI({
    a <- get_answers(); r <- role_d()
    fluidRow(
      column(3, stat_card(fmt_share(mean(r$Job_Status == "Replaced") * 100), "of these jobs were replaced",
                          paste("Everyone:", fmt_share(mean(data$Job_Status == "Replaced") * 100)))),
      column(3, stat_card(fmt_pct(mean(r$Salary_Change_Pct)), "average salary change",
                          paste("Everyone:", fmt_pct(mean(data$Salary_Change_Pct))))),
      column(3, stat_card(fmt_pct(mean(r$Productivity_Change_Pct)), "average productivity change",
                          paste("Everyone:", fmt_pct(mean(data$Productivity_Change_Pct))))),
      column(3, stat_card(sprintf("%.1f", mean(r$Job_Satisfaction)), "average job satisfaction",
                          sprintf("Everyone: %.1f", mean(data$Job_Satisfaction))))
    )
  })
  
  output$outlook_text <- renderUI({
    a <- get_answers(); r <- role_d(); m <- match_d()
    rep_role <- mean(r$Job_Status == "Replaced") * 100
    rep_all  <- mean(data$Job_Status == "Replaced") * 100
    risk <- if (rep_role > rep_all + 3) "higher than average" else if (rep_role < rep_all - 3) "lower than average" else "about average"
    
    opener <- switch(a$feel,
                     Worried = if (risk == "lower than average") "Some good news: " else "It makes sense to plan ahead: ",
                     Excited = "Here's what the data says about your plans: ",
                     "Here's the big picture: ")
    
    level_line <- if (a$ai != "Not sure") {
      lv <- filter(r, AI_Adoption_Level == a$ai)
      if (nrow(lv) > 0) sprintf(
        "Among %ss at workplaces with %s AI adoption (%d people), salaries changed by %s on average and %s of jobs were replaced.",
        a$role, tolower(a$ai), nrow(lv), fmt_pct(mean(lv$Salary_Change_Pct)),
        fmt_share(mean(lv$Job_Status == "Replaced") * 100))
    } else {
      best <- r %>% group_by(AI_Adoption_Level) %>% summarise(s = mean(Salary_Change_Pct), .groups = "drop") %>%
        arrange(desc(s))
      sprintf("You weren't sure how much AI you'd use. For %ss, salaries grew most at workplaces with %s AI adoption (%s).",
              a$role, tolower(as.character(best$AI_Adoption_Level[1])), fmt_pct(best$s[1]))
    }
    
    div(class = "finding",
        p(opener, sprintf("the data has %d %ss across all industries, and %d in %s.",
                          nrow(r), a$role, nrow(m), a$industry),
          sprintf("Their risk of being replaced is %s (%s vs %s for everyone).",
                  risk, fmt_share(rep_role), fmt_share(rep_all))),
        p(level_line),
        if (nrow(m) < 20) p(em(sprintf("Heads up: only %d people match both your job and your industry, so most results use your job across all industries.", nrow(m)))),
        p("Use the tabs above to dig into salary, job security, productivity, and how your career compares.")
    )
  })
  
  output$outlook_plot <- renderPlot({
    a <- get_answers()
    groups <- bind_rows(
      mutate(role_d(), Group = paste0(a$role, "s (all industries)")),
      mutate(match_d(), Group = paste0(a$role, "s in ", a$industry)),
      mutate(ind_d(),  Group = paste("Everyone in", a$industry)),
      mutate(data,     Group = "Everyone")
    )
    groups$Group <- factor(groups$Group, levels = rev(unique(groups$Group)))
    ggplot(groups, aes(y = Group, fill = Job_Status)) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = status_colors) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = "Share of employees", y = NULL, fill = "Job status") +
      my_theme
  })
  
  # ---------------- Salary ----------------
  output$salary_box <- renderPlot({
    a <- get_answers()
    level_box(role_d(), "Salary_Change_Pct", "Salary change (%)", a$ai) +
      labs(title = paste0(a$role, "s: salary change by AI adoption level"),
           subtitle = if (a$ai != "Not sure") "Orange = the AI level you expect")
  })
  
  output$salary_finding1 <- renderUI({
    a <- get_answers(); r <- role_d()
    s <- r %>% group_by(AI_Adoption_Level) %>% summarise(m = mean(Salary_Change_Pct), .groups = "drop")
    div(class = "finding", strong("Key finding: "),
        sprintf("for %ss, average salary change was %s.", a$role,
                paste0(s$AI_Adoption_Level, " AI ", fmt_pct(s$m), collapse = ", ")),
        sprintf(" Across all jobs it was %s.", fmt_pct(mean(data$Salary_Change_Pct))))
  })
  
  output$salary_scatter <- renderPlot({
    a <- get_answers()
    ggplot(role_d(), aes(x = Salary_Before_AI, y = Salary_After_AI, color = AI_Adoption_Level)) +
      annotate("rect", xmin = a$salary * 0.85, xmax = a$salary * 1.15, ymin = -Inf, ymax = Inf,
               fill = you_color, alpha = 0.15) +
      geom_vline(xintercept = a$salary, color = you_color, size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(size = 2.5, alpha = 0.7) +
      scale_color_manual(values = level_colors) +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Salary before AI", y = "Salary after AI", color = "AI adoption",
           subtitle = "Orange band = within 15% of the salary you hope for \u00b7 above dashed line = earned more") +
      my_theme
  })
  
  output$salary_hover_info <- renderUI({
    get_answers()
    pt <- nearPoints(role_d(), input$salary_hover, threshold = 10, maxpoints = 1)
    if (nrow(pt) == 0) return(helpText("Hover over a dot to see that person."))
    tagList(
      strong(pt$Job_Role), br(), pt$Industry, br(),
      "AI adoption: ", as.character(pt$AI_Adoption_Level), br(),
      "Job status: ", as.character(pt$Job_Status), br(),
      sprintf("%s \u2192 %s", scales::dollar(pt$Salary_Before_AI), scales::dollar(pt$Salary_After_AI)), br(),
      strong(fmt_pct(pt$Salary_Change_Pct))
    )
  })
  
  output$salary_finding2 <- renderUI({
    a <- get_answers()
    near <- filter(role_d(), Salary_Before_AI >= a$salary * 0.85, Salary_Before_AI <= a$salary * 1.15)
    if (nrow(near) == 0) {
      return(div(class = "finding", sprintf("No %ss in the data earned within 15%% of %s. Try a different salary on the 'Your Plans' page.",
                                            a$role, scales::dollar(a$salary))))
    }
    div(class = "finding", strong("Key finding: "),
        sprintf("%d %ss earned within 15%% of your target (%s). Their salary changed by %s on average, and %s of them earned more after AI.",
                nrow(near), a$role, scales::dollar(a$salary), fmt_pct(mean(near$Salary_Change_Pct)),
                fmt_share(mean(near$Salary_After_AI > near$Salary_Before_AI) * 100)))
  })
  
  # ---------------- Job Security ----------------
  output$security_by_level <- renderPlot({
    a <- get_answers()
    ggplot(role_d(), aes(y = AI_Adoption_Level, fill = Job_Status)) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = status_colors) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_discrete(drop = FALSE) +
      labs(x = paste0("Share of ", a$role, "s"), y = "AI adoption level", fill = "Job status") +
      my_theme
  })
  
  output$security_finding1 <- renderUI({
    a <- get_answers()
    s <- role_d() %>% group_by(AI_Adoption_Level) %>%
      summarise(rep = mean(Job_Status == "Replaced") * 100, .groups = "drop")
    div(class = "finding", strong("Key finding: "),
        sprintf("share of %ss replaced: %s.", a$role,
                paste0(s$AI_Adoption_Level, " AI ", fmt_share(s$rep), collapse = ", ")))
  })
  
  output$security_roles <- renderPlot({
    a <- get_answers()
    rs <- role_table(ind_d())
    rs$Job_Role <- reorder(rs$Job_Role, rs$Replaced_Pct)
    ggplot(rs, aes(x = Replaced_Pct, y = Job_Role, fill = Job_Role == a$role)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = fmt_share(Replaced_Pct)), hjust = -0.15) +
      scale_fill_manual(values = c(`FALSE` = other_color, `TRUE` = you_color)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = paste("Share of jobs replaced in", a$industry, "(%)"), y = NULL,
           subtitle = "Orange = your job") +
      my_theme
  })
  
  output$security_finding2 <- renderUI({
    a <- get_answers()
    rs <- role_table(ind_d()) %>% arrange(Replaced_Pct)
    rank <- which(rs$Job_Role == a$role)
    div(class = "finding", strong("Key finding: "),
        sprintf("in %s, %s is the %s safest of %d jobs (%s replaced). Safest: %s (%s).",
                a$industry, a$role, scales::ordinal(rank), nrow(rs),
                fmt_share(rs$Replaced_Pct[rank]), rs$Job_Role[1], fmt_share(rs$Replaced_Pct[1])),
        em(" These are small groups, so treat differences of a few percent with care."))
  })
  
  # ---------------- Productivity & Satisfaction ----------------
  output$prod_box <- renderPlot({
    a <- get_answers()
    level_box(role_d(), "Productivity_Change_Pct", "Productivity change (%)", a$ai)
  })
  
  output$sat_box <- renderPlot({
    a <- get_answers()
    level_box(role_d(), "Job_Satisfaction", "Job satisfaction", a$ai)
  })
  
  output$prodsat_finding <- renderUI({
    a <- get_answers()
    s <- role_d() %>% group_by(AI_Adoption_Level) %>%
      summarise(pr = mean(Productivity_Change_Pct), sat = mean(Job_Satisfaction), .groups = "drop")
    div(class = "finding", strong("Key finding: "),
        sprintf("for %ss, productivity changed by %s. Average satisfaction was %s.", a$role,
                paste0(s$AI_Adoption_Level, " AI ", fmt_pct(s$pr), collapse = ", "),
                paste0(s$AI_Adoption_Level, " ", sprintf("%.1f", s$sat), collapse = ", ")))
  })
  
  # ---------------- Compare Careers ----------------
  compare_data <- reactive({
    a <- get_answers()
    if (input$cmp_scope == "industry") role_table(ind_d()) else role_table(data)
  })
  
  output$compare_plot <- renderPlot({
    a <- get_answers()
    info <- priorities[[input$cmp_metric]]
    rs <- compare_data()
    score <- if (info$higher_is_better) rs[[info$var]] else -rs[[info$var]]
    rs$Job_Role <- reorder(rs$Job_Role, score)
    ggplot(rs, aes(x = .data[[info$var]], y = Job_Role, fill = Job_Role == a$role)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(.data[[info$var]], 1)), hjust = -0.15) +
      scale_fill_manual(values = c(`FALSE` = other_color, `TRUE` = you_color)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = info$label, y = NULL, subtitle = "Best at the top \u00b7 orange = your job") +
      my_theme
  })
  
  output$compare_finding <- renderUI({
    a <- get_answers()
    info <- priorities[[input$cmp_metric]]
    rs <- compare_data()
    rs <- if (info$higher_is_better) arrange(rs, desc(.data[[info$var]])) else arrange(rs, .data[[info$var]])
    rank <- which(rs$Job_Role == a$role)
    top3 <- setdiff(rs$Job_Role, a$role)[1:min(3, nrow(rs) - 1)]
    div(class = "finding", strong("Key finding: "),
        sprintf("ranked by %s, %s comes %s out of %d jobs.", tolower(info$label), a$role,
                scales::ordinal(rank), nrow(rs)),
        if (rank > 1) sprintf(" If this matters most to you, also look at: %s.", paste(top3, collapse = ", ")))
  })
  
  output$compare_table <- renderTable({
    a <- get_answers()
    compare_data() %>%
      arrange(desc(Job_Role == a$role), Job_Role) %>%
      transmute(`Job role`                 = ifelse(Job_Role == a$role, paste(Job_Role, "(you)"), as.character(Job_Role)),
                Employees,
                `Avg salary change (%)`    = Salary_Change_Pct,
                `Avg productivity (%)`     = Productivity_Change_Pct,
                `Avg satisfaction`         = Job_Satisfaction,
                `Replaced (%)`             = Replaced_Pct,
                `Unchanged (%)`            = Unchanged_Pct)
  }, digits = 1)
  
  # ---------------- Data ----------------
  data_shown <- reactive({
    switch(input$data_scope, role = role_d(), match = match_d(), industry = ind_d(), all = { get_answers(); data })
  })
  
  output$data_table <- renderTable({
    head(data_shown(), max(1, input$n_rows, na.rm = TRUE))
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("ai_jobs_", input$data_scope, "_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(data_shown(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
