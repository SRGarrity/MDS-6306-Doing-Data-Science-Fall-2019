library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("(New) Old Faithful App!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      #Copy the line below to make a select box 
      selectInput("select", label = h3("Eruption or Waiting Time"), 
                  choices = list("Eruptions" = "eruptions", "Waiting" = "waiting"), 
                  selected = 1),
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)