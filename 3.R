# Load the dataset
data <- read_excel("dataset_MyFoodData.xlsx")

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Food & Health Tracker", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Food Entry", tabName = "food_entry", icon = icon("utensils")),
      menuItem("Profile", tabName = "profile", icon = icon("user")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"),
               menuSubItem("Notifications", tabName = "settings_notifications"),
               menuSubItem("Privacy", tabName = "settings_privacy")
      ),
      menuItem("Logout", tabName = "logout", icon = icon("sign-out"))
    ),
    width = 300
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          .content-wrapper {
            background-color: #F0FFF0 !important; /* Body background color */
          }
          .main-footer {
            background-color: #2E8B57 !important; /* Footer background color */
            color: #F0FFF0 !important; /* Footer text color */
          }
          .main-header .logo {
            background-color: #2E8B57 !important; /* Logo background color */
          }
          .main-header .logo:hover {
            background-color: #228B22 !important; /* Logo background color on hover */
          }
        ")
      )
    ),
    tabItems(
      # Home tab content...
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Daily Goals",
                  solidHeader = TRUE,
                  plotlyOutput("progress_ring"),
                  status = "success",  # Darker shade of pastel green
                  style = "background-color: #98FB98; color: #000000;",  # Pastel green
                  width = 6
                ),
                box(
                  title = "Nutrients Summary",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("indicator_comparison_table"),
                  status = "danger",  # Darker shade of pastel lavender
                  style = "background-color: #FFDAB9; color: #000000;"  # Pastel peach
                )
              ),
              fluidRow(
                box(
                  title = "Calories Distribution",
                  plotlyOutput("calories_bar_chart"),
                  solidHeader = TRUE,
                  status = "info",  # Darker shade of pastel blue
                  style = "background-color: #ADD8E6; color: #000000;"  # Pastel blue
                ),
                box(
                  title = "Daily Calorie Intake Trend",
                  plotlyOutput("calorie_intake_trend"),
                  solidHeader = TRUE,
                  status = "warning",  # Darker shade of pastel salmon
                  style = "background-color: #FFDAB9; color: #000000;"  # Pastel salmon
                ),
                width = 6
              )
      ),
      
      
      # Food Entry tab content...
      tabItem(tabName = "food_entry",
              fluidRow(
               
                box(
                  title = "Add Food Entry",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("food_selection", "Select Food:", choices = unique(data$name)),
                  radioButtons("meal_category", "Meal Category:",
                               choices = c("Breakfast", "Lunch", "Snacks", "Morning Snacks", "Dinner"),
                               inline = TRUE),
                  dateInput("selected_date", "Select Date:", value = Sys.Date(), format = "yyyy-mm-dd"),
                  actionButton("add_food_entry", "Add Food Entry")
                ),
                box(
                  title = "Daily Food Intake",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("daily_food_intake")
                )
              )
      ),
      
      # Profile tab content...
      tabItem(tabName = "profile",
              fluidRow(
                box(
                  title = "User Profile",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  uiOutput("user_profile"),
                  actionButton("edit_profile", "Edit Profile", icon = icon("edit"))
                )
              )
      ),
      
      # Settings tab content...
      tabItem(tabName = "settings",
              fluidRow(
                # Settings tab content...
              )
      ),
      # Logout tab content...
      tabItem(tabName = "logout",
              fluidRow(
                # Logout tab content...
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Data for daily food intake
  daily_food_intake <- reactiveVal(data.frame(Date = as.Date(character()),
                                              Food = character(),
                                              Meal = character(),
                                              Calories = numeric(),
                                              Protein = numeric(),
                                              Fat = numeric(),
                                              Carbohydrate = numeric()
  ))
  
  # Add food entry
  observeEvent(input$add_food_entry, {
    food_entry <- data.frame(Date = input$selected_date,
                             Food = input$food_selection,
                             Meal = input$meal_category,
                             Calories = data$Calories[data$name == input$food_selection],
                             Protein = data$Protein[data$name == input$food_selection],
                             Fat = data$`Fat`[data$name == input$food_selection],
                             Carbohydrate = data$`Carbohydrate`[data$name == input$food_selection]
    )
    daily_food_intake(rbind(daily_food_intake(), food_entry))
    
    # Update plots based on new food entry
    output$calories_bar_chart <- renderPlotly({
      req(input$selected_date)
      filtered_data <- daily_food_intake()[daily_food_intake()$Date == input$selected_date, ]
      calories_by_meal <- aggregate(Calories ~ Meal, data = filtered_data, FUN = sum)
      plot_ly(data = calories_by_meal, x = ~Meal, y = ~Calories, type = 'bar', marker = list(color = '#2E8B57'))
    })
    
    output$calorie_intake_trend <- renderPlotly({
      # Filter daily food intake data for unique dates
      unique_dates <- unique(daily_food_intake()$Date)
      
      # Calculate total calories consumed for each date
      daily_calories <- sapply(unique_dates, function(date) {
        sum(daily_food_intake()[daily_food_intake()$Date == date, "Calories"])
      })
      
      # Create a plotly scatter plot for daily calorie intake
      plot_ly(x = unique_dates, y = daily_calories, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#228B22'), marker = list(color = '#228B22')) %>%
        layout(
          title = "Daily Calorie Intake Trend",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Calories Intake"),
          hoverinfo = "text",
          hovertext = paste("Date: ", unique_dates, "<br>Daily Calories Intake: ", daily_calories)
        )
    })
    
    
    
    output$progress_ring <- renderPlotly({
      req(input$selected_date)
      
      # Filter daily food intake data for the selected date
      filtered_data <- daily_food_intake()[daily_food_intake()$Date == input$selected_date, ]
      
      # Calculate total calories consumed for the selected date
      total_calories <- sum(filtered_data$Calories)
      
      # Retrieve the daily goal from input
      daily_goal <- 1500
      
      # Calculate remaining calories
      remaining_calories <- max(0, daily_goal - total_calories)
      
      # Calculate progress percentage
      progress_percent <- round((total_calories / daily_goal) * 100, 1)
      
      # Create a pie chart to visualize the calorie consumption progress
      plot_ly(
        labels = c("Remaining Calories", "Calories Consumed"),
        values = c(remaining_calories, total_calories),
        type = "pie",
        marker = list(colors = c("#F0FFF0", "#2E8B57")),
        textinfo = "label+percent",
        textposition = "inside",
        hole = 0.6
      ) %>%
        layout(
          title = paste("Calorie Consumption Progress (", progress_percent, "%)"),
          showlegend = FALSE
        )
    })
    
    # Update indicator comparison table
    output$indicator_comparison_table <- renderTable({
      # Sum up nutrient values for each day
      daily_summary <- aggregate(cbind(Calories, Protein, Fat, Carbohydrate) ~ Date, data = daily_food_intake(), FUN = sum)
      
      # Format the Date column
      daily_summary$Date <- format(daily_summary$Date, "%Y-%m-%d")
      
      daily_summary
    })
  }) 
  
  output$daily_food_intake <- renderUI({
    dates <- unique(daily_food_intake()$Date)
    tables <- lapply(dates, function(date) {
      filtered_data <- daily_food_intake()[daily_food_intake()$Date == date, ]
      table <- DT::renderDataTable({
        datatable(
          filtered_data,
          options = list(
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            initComplete = JS(
              "function(settings, json) {",
              "$('button.delete-button').on('click', function() {",
              "var table = $(this).closest('table').DataTable();",
              "var row_index = table.row($(this).closest('tr')).index();",
              "Shiny.setInputValue('delete_row_index', row_index);",
              "});",
              "}"
            )
          ),
          escape = FALSE
        )
      })
      tagList(
        h3(paste("Date:", date)),
        table
      )
    })
    tagList(tables)
  })
  
  # User profile reactive
  user_profile_data <- reactiveVal(
    list(
      Full_Name = "Tanushree",
      Age = 21,
      Height = 150,
      Weight = 50,
      Daily_Calories_Goal = 2000
    )
  )
  
  # Display user profile
  output$user_profile <- renderText({
    profile <- user_profile_data()
    paste("Full Name:", profile$Full_Name, "<br>",
          "Age:", profile$Age, "<br>",
          "Height:", profile$Height, "cm<br>",
          "Weight:", profile$Weight, "kg<br>",
          "Daily Calorie Goal:", profile$Daily_Calories_Goal)
  })
  
  # Edit profile button handler
  observeEvent(input$edit_profile, {
    showModal(
      modalDialog(
        textInput("edit_full_name", "Full Name:", value = user_profile_data()$Full_Name),
        numericInput("edit_age", "Age:", value = user_profile_data()$Age, min = 0),
        numericInput("edit_height", "Height (cm):", value = user_profile_data()$Height, min = 0),
        numericInput("edit_weight", "Weight (kg):", value = user_profile_data()$Weight, min = 0),
        numericInput("edit_daily_calories_goal", "Daily Calorie Goal:", value = user_profile_data()$Daily_Calories_Goal, min = 0),
        footer = tagList(
          actionButton("save_profile", "Save"),
          modalButton("Close")
        )
      )
    )
  })
  
  # Save profile changes
  observeEvent(input$save_profile, {
    user_profile_data(list(
      Full_Name = input$edit_full_name,
      Age = input$edit_age,
      Height = input$edit_height,
      Weight = input$edit_weight,
      Daily_Calories_Goal = input$edit_daily_calories_goal
    ))
    removeModal()
  })
  # Display user profile
  output$user_profile <- renderUI({
    profile <- user_profile_data()
    tags$div(
      h3("User Profile"),
      tags$table(
        tags$tr(
          tags$td("Full Name:"),
          tags$td(profile$Full_Name)
        ),
        tags$tr(
          tags$td("Age:"),
          tags$td(profile$Age)
        ),
        tags$tr(
          tags$td("Height:"),
          tags$td(paste(profile$Height, "cm"))
        ),
        tags$tr(
          tags$td("Weight:"),
          tags$td(paste(profile$Weight, "kg"))
        ),
        tags$tr(
          tags$td("Daily Calorie Goal:"),
          tags$td(profile$Daily_Calories_Goal)
        )
      )
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


