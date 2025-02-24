library(shiny)
library(shinyBS)
library(mapview)

myData <- myData <- readRDS("input/myData.RDS")

# Define UI for application
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .section-heading { 
        font-weight: bold; 
        font-size: 16px; 
        color: #333; 
        padding-top: 10px;
        padding-bottom: 5px;
        margin-bottom: 10px;
        border-bottom: 1px solid #ddd;
      }
      .sidebar { width: 200px; }
      .main { width: calc(100% - 200px); }
    "))
  ),
  tags$div(class = "header", checked = NA,),
  
  titlePanel(
    div(
      h1("Dynamic social vulnerability mapping", align = "center"),
      h4(em("Assessing changes of vulnerable population's exposure to climate risks in timely situations"), align = "center"),
      style = "text-align: center;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Crisis section
      div(class = "section-heading", "Crisis",img(src = "logo-info.png")),
      selectInput("crisisChoice", "",
                  choices = c("High", "Medium", "Low")),
      selectInput("crisisChoice", "",
                  choices = c("Weekday", "Weekend")),
      selectInput("crisisChoice", "",
                  choices = c("Regular", "Holiday")),
      
      # Reference section
      div(class = "section-heading", "Reference"),
      selectInput("crisisChoice", "",
                  choices = c("High", "Medium", "Low")),
      selectInput("crisisChoice", "",
                  choices = c("Weekday", "Weekend")),
      selectInput("crisisChoice", "",
                  choices = c("Regular", "Holiday")),
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      img(src='logo-info.png', align = "right"),
      ### the rest of your code
      plotOutput("plot") # The plot will take the whole main panel
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Placeholder for dynamic plot output based on sidebar input
  output$plot <- renderPlot({
    # Your plot generation code depending on input$crisisChoice or input$referenceChoice
    # For now, we'll just create a blank plot.
    plot(iris$Sepal.Length)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
