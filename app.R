## app.R ##
library(shiny) # core shiny framework
library(shinydashboard) # layout package
library(dplyr) # data manipulation package, e.g. sorting, filtering, summarizing
library(ggplot2) # static charting package
library(DT) # interactive data tables
library(scales) # for percentage formatting
library(plotly) # interactive charting package

source("R/get_sejm_data.R")

# --- UI -----------------------------------------------------------------------
# this section defines the layout and static components of the dashboard
ui <- dashboardPage(
  skin = "blue",
  
  # 1. Header
  # adding a refresh button to the header
  dashboardHeader(title = "Sejmometr Dashboard",
                  tags$li(class = "dropdown", # using tags$li to place button on right side as a dashboardHeader"dropdown" item (even though it's not a dropdown)
                          actionButton("refresh_btn", "Refresh Data", icon = icon("sync"),
                                       style = "margin-top: 8px; margin-right: 10px; color: #333;")
                  )
  ),
  
  # 2. Sidebar
  # adding filters to the sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "dashboard", icon = icon("filter")), # dummy menu item for filters section
      
      # Club Multi-Select Filter
      selectInput("club_filter", "Select Club (Empty = All):", # dynamic choices populated in server
                  choices = NULL, # initially NULL
                  multiple = TRUE), # allow multiple selections
      
      # Attendance Rate Slider Filter
      sliderInput("attendance_filter", "Attendance Rate (%):",
                  min = 0, max = 100, value = c(0, 100)) # full range by default
    )
  ),
  
  # 3. Body
  dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 120px}"))), # custom CSS injection to make metric boxes taller
    
    tabItems(
      tabItem(tabName = "dashboard",
              
              # SECTION 1: Welcome & Explanations (The "Text Field")
              fluidRow(
                box(
                    title = "About Sejmometr", 
                    status = "primary", # this sets the color of the box
                    solidHeader = TRUE, # this controls the style of the header (solid color)
                    width = 12, # full bootstrap width
                    collapsible = TRUE, # allow collapsing
                    collapsed = FALSE, # start expanded
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
              fluidRow( # these are placeholders for the 3 value boxes generated in the server
                valueBoxOutput("box_avg_attendance", width = 4),
                valueBoxOutput("box_avg_cost", width = 4),
                valueBoxOutput("box_total_missed", width = 4)
              ),
              
              # SECTION 3: Visualizations
              fluidRow(
                box(
                  title = "Global Correlation: Age vs. Attendance", status = "primary", solidHeader = TRUE, width = 7,
                  plotlyOutput("scatter_plot", height = "350px"), # plotly chart used for interactivity
                  footer = "Hover over points to see MP details. This chart shows all MPs."
                ),
                
                box(
                  title = "Global Attendance by Club", status = "primary", solidHeader = TRUE, width = 5,
                  plotOutput("club_plot", height = "350px") # standard static ggplot2 chart
                )
              ),
              
              # SECTION 4: Data Table
              fluidRow(
                box(
                  title = "Filtered MP List", status = "primary", width = 12,
                  DTOutput("mp_table") # using DT package for interactive table
                )
              )
      )
    )
  )
)

# --- SERVER -------------------------------------------------------------------
server <- function(input, output, session) { # this function contains all the reactive logic
  
  # 1. Reactive Data Reading
  # this will automatically re-read the CSV data file every second
  mps_data <- reactiveFileReader(1000, session, "data/mps_processed.csv", read.csv)
  
  # 2. Populate Club Choices
  # this observer updates the club filter choices based on available data when data is (re)loaded
  observeEvent(mps_data(), {
    data <- mps_data()
    updateSelectInput(session, "club_filter",
                      choices = sort(unique(data$club))) # unique club names sorted alphabetically
  })
  
  # 3. Filter Logic
  filtered_data <- reactive({ # this creates a live filtered dataset based on user inputs
    req(mps_data()) # make sure data is available
    df <- mps_data()
    
    if (!is.null(input$club_filter)) { # only filter by club if user has selected any
      df <- df %>% filter(club %in% input$club_filter) # checks if club is in selected list
    }
    
    # this filters by attendance rate range
    # it's a vector of two values: c(min, max) divided by 100 to convert from percentage to decimal (100% -> 1.0)
    df <- df %>% filter(attendance_rate >= input$attendance_filter[1]/100,
                        attendance_rate <= input$attendance_filter[2]/100)
    
    df
  })
  
  # 4. Refresh Button Logic
  observeEvent(input$refresh_btn, { # runs when the refresh button is clicked
    withProgress(message = 'Updating Data from Sejm API...', value = 0, { # shows a progress bar
      # this function will be called by update_sejm_data to report progress
      prog_update <- function(val, msg) {
        incProgress(amount = 0, detail = msg)
        setProgress(val)
      }
      tryCatch({ # this will report on progress results and handle errors nicely instead of crashing the app
        update_sejm_data(progress_callback = prog_update)
        showNotification("Data updated successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating data:", e$message), type = "error")
      })
    })
  })
  
  # --- OUTPUTS ---
  # this section sends outputs to the UI components defined above

  # 1. Value Box: Average Attendance
  output$box_avg_attendance <- renderValueBox({
    df <- filtered_data()
    # calculate mean removing NAs; if no rows, set to 0
    avg_att <- if(nrow(df) > 0) mean(df$attendance_rate, na.rm = TRUE) else 0
    
    # create the value box with conditional coloring
    valueBox(
      percent(avg_att, accuracy = 0.1), # display as percentage with 0.1% accuracy
      "Avg Attendance (Selection)", 
      icon = icon("user-check"),
      # conditional logic for color based on attendance rate
      color = if(avg_att > 0.9) "green" else if(avg_att > 0.7) "orange" else "red"
    )
  })
  
  # 2. Value Box: Average Cost per Vote
  output$box_avg_cost <- renderValueBox({
    df <- filtered_data()
    # calculate mean removing NAs; if no rows, set to 0
    avg_cost <- if(nrow(df) > 0) mean(df$cost_per_vote, na.rm = TRUE) else 0
    
    # create the value box with a fixed color
    valueBox(
      paste(round(avg_cost, 0), "PLN"), # display rounded to nearest whole PLN
      "Avg Cost/Vote (Selection)", 
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })
  
  # 3. Value Box: Total Missed Votes
  output$box_total_missed <- renderValueBox({
    df <- filtered_data()
    # sum total missed removing NAs
    total_missed <- sum(df$totalMissed, na.rm = TRUE)
    
    # create the value box with a fixed color
    valueBox(
      format(total_missed, big.mark = ","), # format with commas for thousands
      "Missed Votes (Selection)", 
      icon = icon("times-circle"),
      color = "red"
    )
  })
  
  # 4. Scatter Plot: Age vs. Attendance
  output$scatter_plot <- renderPlotly({
    df <- mps_data() # this plot shows all MPs, not just filtered ones
    
    # define the ggplot
    p <- ggplot(df, aes(x = age, y = attendance_rate, color = club,
                        text = paste("Name:", fullName, 
                                     "<br>Club:", club,
                                     "<br>Age:", age,
                                     "<br>Attendance:", percent(attendance_rate, 0.1)))) +
      geom_point(alpha = 0.7, size = 2) + # dots with some transparency
      scale_y_continuous(labels = percent_format()) + # format y-axis as percentage
      theme_minimal() +
      labs(x = "Age (Years)", y = "Attendance Rate", color = "Club") +
      theme(legend.position = "none") # hide legend for clarity
    
    ggplotly(p, tooltip = "text") # convert to plotly for interactive tooltips
  })
  
  # 5. Bar Plot: Average Attendance by Club
  output$club_plot <- renderPlot({
    df <- mps_data() # this plot shows all MPs, not just filtered ones

    # summary chain: group by club -> calculate mean attendance -> arrange descending
    df_summary <- df %>% 
    group_by(club) %>% 
    summarise(mean_att = mean(attendance_rate, na.rm = TRUE)) %>% 
    arrange(desc(mean_att))
    
    # create the ggplot bar chart
    ggplot(df_summary, aes(x = reorder(club, mean_att), y = mean_att, fill = club)) +
      geom_col(show.legend = FALSE) + coord_flip() + # horizontal bars
      geom_text(aes(label = percent(mean_att, accuracy = 0.1)), # add text labels
                hjust = 1.2,
                color = "white", fontface = "bold") +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      theme_minimal() + labs(x = "", y = "Avg Attendance")
  })
  
  # 6. Data Table: Filtered MPs
  output$mp_table <- renderDT({
    df <- filtered_data() %>% # take filtered data and select relevant columns
      select(fullName, club, age, attendance_rate, totalVotings, totalMissed, cost_per_vote, estimated_salary = monthly_salary)
    
    # create the datatable with options
    datatable(df, options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Name", "Club", "Age", "Attendance", "Total Votes", "Missed", "Cost/Vote", "Est. Monthly Salary")) %>%
      formatPercentage('attendance_rate', 1) %>% # format attendance as percentage with 1 decimal
      formatCurrency(c('cost_per_vote', 'estimated_salary'), currency = " PLN ", before = FALSE) # format as currency and place PLN after amount
  })
}

# launch the app
shinyApp(ui, server)