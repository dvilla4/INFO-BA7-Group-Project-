library(shiny)
library(tidyverse)
library(plotly)

health <- read.csv("student-mental-health.csv")

ui <- fluidPage(

    titlePanel("Student Mental Health"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 headerPanel("General Overview"),
                 p("\nThis app describes the results of data collected from a survey conducted to 
                   examine academic variables in comparison to topics of mental health. The 
                   public dataset is titled 'Student Mental Health', and is defined as a 
                   statistical research on the effects of mental health on students' CGPA.\n"),
                 headerPanel("Data"),
                 p("\nAs explained, we are working with a public data set published on Kaggle. 
                   data set contains 101 observations, and 11 variables, 9 variables of which are
                    relevant to out analysis and will be addressing. Variables will include demographics,
                     academic situation, and several mental health topics. These observations were 
                     collected in 2020, conducted as an online survey primarily and solely catered
                     towards university students.\n"),
                 headerPanel("Purpose"),
                 p("\nThe purpose of examining this data set is to find an association between academic 
                   situation and state of mental health. As the data has been collected from 
                   university students, they will be our main audience to address. However, the findings 
                   from this report can also apply to other audiences in current academic situations.\n")),
        
        tabPanel("Page #1",
                 headerPanel("Sample Observations"),
                 p("Use the slider to observe samples of observations from the data set."),
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("n", "Number of Observations:",
                                 min = 5,
                                 max = 100,
                                 value = 10)
                   ),
                   mainPanel(
                     tableOutput("table1")
                   )
                 )),
        
        tabPanel("Page #2"),
        
        tabPanel("Page #3")
        
      )
    )

)


server <- function(input, output) {
  
  output$table1 <- renderTable({
    health %>% 
      sample_n(input$n)
  })
  
}


shinyApp(ui = ui, server = server)

-------------------------------------------------------------------------------------------------------------------

library(shiny)

# Define UI for Shiny app
ui <- fluidPage(
  
  # Application title
  titlePanel("Student Mental Health"),
  
  # Sidebar with tabs
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Gender",
                 selectInput(inputId = "gender", label = "Choose your gender", 
                             choices = c("Female", "Male"), selected = "Female")),
        tabPanel("Anxiety and Year of Study",
                 selectInput(inputId = "anxiety", label = "Do you have Anxiety?",
                             choices = c("Yes", "No")),
                 selectInput(inputId = "year", label = "What is your current year of study?",
                             choices = c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5+"),
                             selected = "Year 1")),
        tabPanel("Treatment",
                 selectInput(inputId = "treatment", label = "Did you seek any specialist for a treatment?",
                             choices = c("Yes", "No")))
      )
    ),
    
    # Show data table in main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput(outputId = "table")),
        tabPanel("Summary", verbatimTextOutput(outputId = "summary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load data from a CSV file
  data <- read.csv("C:/Users/villa/OneDrive/Documents/Student Mental Health.csv", header = TRUE)
  
  # Subset data based on selected inputs
  subset_data <- reactive({
    data_subset <- data
    
    # Filter data based on gender
    if (!is.null(input$gender) & input$gender != "All") {
      data_subset <- subset(data_subset, Choose.your.gender == input$gender)
    }
    
    # Filter data based on anxiety and year of study
    if (!is.null(input$anxiety) & input$anxiety != "All") {
      data_subset <- subset(data_subset, Do.you.have.Anxiety. == input$anxiety)
    }
    
    if (!is.null(input$year) & input$year != "All") {
      data_subset <- subset(data_subset, Your.current.year.of.Study == input$year)
    }
    
    # Filter data based on treatment
    if (!is.null(input$treatment) & input$treatment != "All") {
      data_subset <- subset(data_subset, Did.you.seek.any.specialist.for.a.treatment. == input$treatment)
    }
    
    return(data_subset)
  })
  
  # Display data table
  output$table <- renderTable({
    subset_data()
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    summary(subset_data())
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
