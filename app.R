## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(plotly)

source("R/get_sejm_data.R")

# --- UI -----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  # 1. Header
  dashboardHeader(title = "Sejmometr Dashboard",
                  tags$li(class = "dropdown",
                          actionButton("refresh_btn", "Refresh Data", icon = icon("sync"),
                                       style = "margin-top: 8px; margin-right: 10px; color: #333;")
                  )
  ),
  
  # 2. Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "dashboard", icon = icon("filter")),
      
      selectInput("club_filter", "Select Club (Empty = All):", 
                  choices = NULL, 
                  multiple = TRUE),
      
      sliderInput("attendance_filter", "Attendance Rate (%):",
                  min = 0, max = 100, value = c(0, 100))
    )
  ),
  
  # 3. Body
  dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 120px}"))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              
              # SECTION 1: Welcome & Explanations (The "Text Field")
              fluidRow(
                box(
                    title = "About Sejmometr", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    "Welcome to the Sejm Activity Dashboard. This tool visualizes the performance of MPs in the current term.",
                    tags$br(), tags$br(),
                    tags$strong("Metric Definitions:"),
                    tags$ul(
                        tags$li(tags$strong("Attendance Rate:"), "Percentage of votings where the MP was present or had an excused absence."),
                        tags$li(tags$strong("Cost per Vote:"), "A calculated index: (Estimated Total Salary + Bonuses) / Total Votes Cast. High cost indicates low activity relative to earnings."),
                        tags$li(tags$strong("Missed Votes:"), "Total number of votes missed without a valid excuse.")
                    ),
                    tags$hr(), # Horizontal line separator
                    tags$strong("Important Note on Salaries:"),
                    tags$p("The salary figures shown in this dashboard are approximate estimates intended for comparative purposes only.", style = "font-size: 13px; color: #555;"),
                    tags$ul(style = "font-size: 13px; color: #555;",
                        tags$li("Estimates include: Base salary (~13,467.92 PLN gross/mo) + Parliamentary allowance (~4,208.73 PLN gross/mo) + calculated bonuses for committee functions (Chairman +20%, Deputy +15%)."),
                        tags$li("Estimates DO NOT include: Housing allowance (up to 4,500 PLN/mo), Office lump-sum (up to 23,310 PLN/mo), or other benefits (e.g., free travel).")
                    ),
                    tags$p(
                        style = "font-size: 13px; color: #555;",
                        "For exact figures, please refer to the official MP asset declarations here:",
                        tags$a(href = "https://www.sejm.gov.pl/Sejm10.nsf/poslowie.xsp", "List of MPs (Sejm.gov.pl)", target = "_blank")
                    )
                )
              ),
              
              # SECTION 2: Key Metrics (Standard Value Boxes)
              fluidRow(
                valueBoxOutput("box_avg_attendance", width = 4),
                valueBoxOutput("box_avg_cost", width = 4),
                valueBoxOutput("box_total_missed", width = 4)
              ),
              
              # SECTION 3: Visualizations
              fluidRow(
                box(
                  title = "Global Correlation: Age vs. Attendance", status = "primary", solidHeader = TRUE, width = 7,
                  plotlyOutput("scatter_plot", height = "350px"),
                  footer = "Hover over points to see MP details. This chart shows all MPs."
                ),
                
                box(
                  title = "Global Attendance by Club", status = "primary", solidHeader = TRUE, width = 5,
                  plotOutput("club_plot", height = "350px")
                )
              ),
              
              # SECTION 4: Data Table
              fluidRow(
                box(
                  title = "Filtered MP List", status = "primary", width = 12,
                  DTOutput("mp_table")
                )
              )
      )
    )
  )
)

# --- SERVER -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Reactive Data Reading
  mps_data <- reactiveFileReader(1000, session, "data/mps_processed.csv", read.csv)
  
  # 2. Populate Club Choices
  observeEvent(mps_data(), {
    data <- mps_data()
    updateSelectInput(session, "club_filter",
                      choices = sort(unique(data$club)))
  })
  
  # 3. Filter Logic
  filtered_data <- reactive({
    req(mps_data())
    df <- mps_data()
    
    if (!is.null(input$club_filter)) {
      df <- df %>% filter(club %in% input$club_filter)
    }
    
    df <- df %>% filter(attendance_rate >= input$attendance_filter[1]/100,
                        attendance_rate <= input$attendance_filter[2]/100)
    
    df
  })
  
  # 4. Refresh Button Logic
  observeEvent(input$refresh_btn, {
    withProgress(message = 'Updating Data from Sejm API...', value = 0, {
      prog_update <- function(val, msg) {
        incProgress(amount = 0, detail = msg)
        setProgress(val)
      }
      tryCatch({
        update_sejm_data(progress_callback = prog_update)
        showNotification("Data updated successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating data:", e$message), type = "error")
      })
    })
  })
  
  # --- OUTPUTS ---
  
  output$box_avg_attendance <- renderValueBox({
    df <- filtered_data()
    avg_att <- if(nrow(df) > 0) mean(df$attendance_rate, na.rm = TRUE) else 0
    
    valueBox(
      percent(avg_att, accuracy = 0.1),
      "Avg Attendance (Selection)", 
      icon = icon("user-check"),
      color = if(avg_att > 0.9) "green" else if(avg_att > 0.7) "orange" else "red"
    )
  })
  
  output$box_avg_cost <- renderValueBox({
    df <- filtered_data()
    avg_cost <- if(nrow(df) > 0) mean(df$cost_per_vote, na.rm = TRUE) else 0
    
    valueBox(
      paste(round(avg_cost, 0), "PLN"),
      "Avg Cost/Vote (Selection)", 
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })
  
  output$box_total_missed <- renderValueBox({
    df <- filtered_data()
    total_missed <- sum(df$totalMissed, na.rm = TRUE)
    
    valueBox(
      format(total_missed, big.mark = ","),
      "Missed Votes (Selection)", 
      icon = icon("times-circle"),
      color = "red"
    )
  })
  
  output$scatter_plot <- renderPlotly({
    df <- mps_data()
    
    p <- ggplot(df, aes(x = age, y = attendance_rate, color = club,
                        text = paste("Name:", fullName, 
                                     "<br>Club:", club,
                                     "<br>Age:", age,
                                     "<br>Attendance:", percent(attendance_rate, 0.1)))) +
      geom_point(alpha = 0.7, size = 2) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal() +
      labs(x = "Age (Years)", y = "Attendance Rate", color = "Club") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$club_plot <- renderPlot({
    df <- mps_data()
    df_summary <- df %>% group_by(club) %>% summarise(mean_att = mean(attendance_rate, na.rm = TRUE)) %>% arrange(desc(mean_att))
    
    ggplot(df_summary, aes(x = reorder(club, mean_att), y = mean_att, fill = club)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      geom_text(aes(label = percent(mean_att, accuracy = 0.1)), 
                hjust = 1.2,
                color = "white", fontface = "bold") +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      theme_minimal() + labs(x = "", y = "Avg Attendance")
  })
  
  output$mp_table <- renderDT({
    df <- filtered_data() %>%
      select(fullName, club, age, attendance_rate, totalVotings, totalMissed, cost_per_vote, estimated_salary = monthly_salary)
    
    datatable(df, options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Name", "Club", "Age", "Attendance", "Total Votes", "Missed", "Cost/Vote", "Est. Monthly Salary")) %>%
      formatPercentage('attendance_rate', 1) %>%
      formatCurrency(c('cost_per_vote', 'estimated_salary'), currency = " PLN ", before = FALSE)
  })
}

shinyApp(ui, server)