

library(shiny)
library(shinyBS)
library(leaflet)
library(ggplot2)
library(dplyr)
library(mapview)
library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)
library(here)

dataPath = here("Data","myData.RDS")
polyPath = here("Data","ph_municity_poly_psa_namria_psgc2020_simpl1.rds")
myData <- readRDS(dataPath)
poly <- readRDS(polyPath)


#-------- MAP generating function
myBivariateMap <- function(myData, crisis, reference){  
  
  # ------ Compute thematic data ---
  data <-  myData %>%
    # --- Compute change of population density/exposure (FB_index) ---
    mutate(FB_index = ((.data[[crisis]] / .data[[reference]]) - 1) * 100) %>% # Rowe's formula
    dplyr::select(psgc_2020, SoVI_index, FB_index) 
  
  # ------  Load geodata and join with thematic data 
  muncity_geo <- st_as_sf(poly)
  muncity_geo <- muncity_geo %>%
    left_join(data, by = c("psgc_2020" = "psgc_2020")) 
  
  # ------  Create quantile categories
  
  # note: must be done as early as possible because if we filter the dataset (e.g remove NAs), 
  # we'll get less entries, which in turn will modify the number of munciplity
  # per category. This becomes problematic when you want to properly compare one map with another
  
  # create 3 buckets for SoVI_index
  (quantiles_SoVI_index <- data %>%
      pull(SoVI_index) %>%
      quantile(probs = seq(0, 1, length.out = 4), na.rm=TRUE) )
  
  # create 3 buckets for FB_index
  (quantiles_FB_index <- data %>%
      pull(FB_index) %>%
      quantile(probs = seq(0, 1, length.out = 4), na.rm=TRUE) )
  # create color scale that encodes the two variables
  # red for SoVI_index and blue for FB_index
  # the special notation with gather is due to readibility reasons
  bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high vulnerability, high exposure
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low vulnerability, high exposure
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium vulnerability, medium exposure
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high vulnerability, low exposure
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low vulnerability, low exposure
  ) %>%
    gather("group", "fill")
  
  ###  ------- Join Color Codes to the Data ---
  
  # cut into groups defined above and join fill
  muncity_geo %<>%
    mutate(
      SoVI_index_quantiles = cut(
        SoVI_index,
        breaks = quantiles_SoVI_index,
        include.lowest = TRUE
      ),
      FB_index_quantiles = cut(
        FB_index,
        breaks = quantiles_FB_index,
        include.lowest = TRUE
      ),
      # by pasting the factors together as numbers we match the groups defined
      # in the tibble bivariate_color_scale
      group = paste(
        as.numeric(SoVI_index_quantiles), "-",
        as.numeric(FB_index_quantiles)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each municipality knows its hex value based on the his SoVI_index and FB_index value
    left_join(bivariate_color_scale, by = "group")
  
  
  # ------  Create color palette for map display
  muncity_geo1 <- muncity_geo
  muncity_geo1$group <- as.factor(muncity_geo1$group)
  muncity_geo1 <- muncity_geo1 %>% na.omit()
  bivariate_color_palette <- c(    
    "#CABED0",  #1-1 # low vulnerability, low exposure
    "#89A1C8",  #1-2
    "#4885C1",  #1-3  # low vulnerability, high exposure
    
    "#BC7C8F",  #2-1
    "#806A8A",  #2-2 # medium vulnerability, medium exposure
    "#435786",  #2-3
    
    "#AE3A4E" ,  #3-1 # high vulnerability, low exposure
    "#77324C",  #3-2
    "#3F2949")  #3-3 # high vulnerability, high exposure
  
  # ------ Compute interactive map 
  (myMap <- mapview(
    muncity_geo1,
    zcol = "group",
    col.regions = bivariate_color_palette,
    alpha.regions = 0.95,
    layer.name = "DSoVI",
    legend = FALSE,
    legend.title = "My Legend",
    legend.subtitle = paste0("Dynamic social vulnerability", " (", crisis, " VS ", reference, ")"),
    legend.cex = 0.8
  )
  )
}
getCase <- function(dayType, weekPart, timeOfDay) {
  # Map inputs to case numbers
  cases <- list(
    "Regular - Weekday - Nighttime" = "case01",
    "Regular - Weekday - Daytime" = "case02",
    "Regular - Weekday - Evening" = "case03",
    "Regular - Weekend - Nighttime" = "case04",
    "Regular - Weekend - Daytime" = "case05",
    "Regular - Weekend - Evening" = "case06",
    "Holiday - Weekday - Nighttime" = "case07",
    "Holiday - Weekday - Daytime" = "case08",
    "Holiday - Weekday - Evening" = "case09",
    "Holiday - Weekend - Nighttime" = "case10",
    "Holiday - Weekend - Daytime" = "case11",
    "Holiday - Weekend - Evening" = "case12"
  )
  
  # Create the key from the input parameters
  key <- paste(dayType, weekPart, timeOfDay, sep = " - ")
  
  # Return the corresponding case number, or a default message if not found
  return(ifelse(!is.null(cases[[key]]), cases[[key]], "Unknown configuration"))
}
#--------


# Define UI for application
ui <- fluidPage(
  
  tags$head(
    tags$title("DSoVI | Dynamic social vulnerability mapping using Facebook data"),
    tags$style(HTML("
    /* Style adjustments for full height and width map */
      
      .section-heading { 
        font-weight: bold; 
        font-size: 16px; 
        color: #333; 
        padding-top: 10px;
        padding-bottom: 5px;
        margin-bottom: 10px;
        border-bottom: 1px solid #ddd;
      }
      .sidebar { width: 300px; }
      .main { width: calc(100% - 300px); }
            .info-icon {
        border: none;         /* Remove border */
        background-color: transparent; /* Remove background */
        padding: 0;           /* Remove padding */
        cursor: pointer;
      }
      .info-icon:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        left: 20px;
        z-index: 1;
        background: black;
        color: white;
        padding: 5px;
        border-radius: 5px;
        font-size: 12px;
        white-space: pre-line;  /* Keep the tooltip text on a single line */
      }
      /* Override Bootstrap button styles */
      .btn:focus,
      .btn:active:focus,
      .btn.active:focus {
        outline: none;
        box-shadow: none;
      }
      .shiny-input-radiogroup .shiny-options-group {
      display: flex;
      flex-direction: row;
      }
      .shiny-input-radiogroup .shiny-options-group .shiny-input-container {
        flex-grow: 1;
      }
    "))
  ),
  tags$div(class = "header", checked = NA,),
  
  titlePanel(
    div(
      h1("Dynamic social vulnerability mapping", align = "center"),
      h4(em("Assessing changes of population exposure and vulnerability to climate risks in timely situations"), align = "center"),
      style = "text-align: center;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "section-heading","Crisis",
      # Crisis section
          actionButton(
            "info", 
            label = icon("info-circle"), 
            class = "info-icon", 
            `data-tooltip` = "Time period of the expected crisis"
          )),
      radioButtons("CrisisChoiceD", label = "",
                   choices = c("Daytime", "Evening", "Nighttime"), selected = "Daytime",
                   inline = TRUE),
      radioButtons("CrisisChoiceW", label = "",
                   choices = c("Weekday", "Weekend"), selected = "Weekday",
                   inline = TRUE),
      radioButtons("CrisisChoiceH", label = "",
                   choices = c("Regular", "Holiday"), selected = "Regular",
                   inline = TRUE),
      
      # Reference section
      div(class = "section-heading", "Reference",actionButton(
        "info", 
        label = icon("info-circle"), 
        class = "info-icon", 
        `data-tooltip` = "Time period of reference serving as baseline for comparison")),
      radioButtons("RefChoiceD", "",
                   choices = c("Daytime", "Evening", "Nighttime"),selected = "Nighttime",inline = TRUE),
      radioButtons("RefchoiceW", "",
                   choices = c("Weekday", "Weekend"),selected = "Weekday",inline = TRUE),
      radioButtons("RefchoiceH", "",
                   choices = c("Regular", "Holiday"),selected = "Regular",inline = TRUE),
      
      # Legend
      div(class = "section-heading", "Legend",actionButton(
        "info", 
        label = icon("info-circle"), 
        class = "info-icon", 
        `data-tooltip` = "Red areas show high levels of social vulnerability combined with a decrease of exposure, while blue areas represent low levels of social vulnerability combined with an increase of exposure. \n On the opposite, violet areas show high levels of social vulnerability combined with an increase of exposure, while grey-pink areas represent low levels of social vulnerability combined with a decrease of exposure.
")),
      plotOutput("bivariatePlot")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      ### the rest of your code
      leafletOutput("map",height = "80vh"),
      uiOutput("warning",width = "100px", height = "100px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  is_equal  = reactiveVal()
  choice1 <- reactive({
    # Assume getCase is a function that takes three parameters and returns a value
    getCase(input$CrisisChoiceH,input$CrisisChoiceW,input$CrisisChoiceD)
  })
  
  choice2 <- reactive({
    # Assume getCase is a function that takes three parameters and returns a value
    getCase(input$RefchoiceH,input$RefchoiceW,input$RefChoiceD)
  })
  
  # Placeholder for dynamic plot output based on sidebar input
  observe({
    if(choice1()!=choice2()){
      output$map <- renderLeaflet({
        myBivariateMap(myData,choice1(), choice2())}@map)
      output$warning <- renderUI({""}) 
    }
    else{
      output$map <- NULL
      output$warning <- renderUI({
        # Use tags$div to combine HTML elements
        tags$div(
          icon("exclamation-triangle", style = "color: red;"), # Warning icon in red
          tags$span(style = "color: red; margin-left: 5px;", "Crisis and Reference levels must be different")
        )
      })
    }
  })
  
  output$bivariatePlot <- renderPlot({
    bivariate_color_scale <- tibble(
      "3 - 3" = "#3F2949", # high vulnerability, high exposure
      "2 - 3" = "#435786",
      "1 - 3" = "#4885C1", # low vulnerability, high exposure
      "3 - 2" = "#77324C",
      "2 - 2" = "#806A8A", # medium vulnerability, medium exposure
      "1 - 2" = "#89A1C8",
      "3 - 1" = "#AE3A4E", # high vulnerability, low exposure
      "2 - 1" = "#BC7C8F",
      "1 - 1" = "#CABED0" # low vulnerability, low exposure
    ) %>%
      gather("group", "fill")
    #435786
    bivariate_color_scale <- tibble(
      group = c("3 - 3", "2 - 3", "1 - 3", "3 - 2", "2 - 2", "1 - 2", "3 - 1", "2 - 1", "1 - 1"),
      fill = c("#CABED0", "#89A1C8", "#4885C1", "#BC7C8F", "#806A8A", "#435786", "#AE3A4E", "#77324C", "#3F2949")
    )
    
    # Convert group to a factor with levels ordered for plotting
    bivariate_color_scale$group <- factor(bivariate_color_scale$group, levels = bivariate_color_scale$group)
    
    # Create a plot_data tibble that maps the color scale onto a grid layout
    plot_data <- expand_grid(
      Exposure = factor(1:3, labels = c("1", "2", "3")),
      Vulnerability = factor(1:3, labels = c("1", "2", "3"))
    ) %>%
      mutate(group = factor(paste(Vulnerability, Exposure, sep = " - "), levels = rev(bivariate_color_scale$group)))
    
    # Now let's create the plot
    ggplot(plot_data, aes(x = Exposure, y = Vulnerability, fill = group)) +
      geom_tile() +  # This creates the squares
      scale_fill_manual(values = bivariate_color_scale$fill) +
      labs(x = "Vulnerability \u2192", y = "Exposure \u2192") +
      theme_minimal() + theme(legend.position = "none") # Removes axes and background
  },height = 200, width = 200)
}

# Run the application 
shinyApp(ui = ui, server = server)

