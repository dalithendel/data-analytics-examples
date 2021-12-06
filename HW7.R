if (!require("knitr")) 
  install.packages("knitr")
library(knitr)
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, dpi = 500)

# Load the packages, install if needed
#install.packages("janitor")
library(readr)        # Used for reading in data
library(dplyr)        # A data management library
library(ggplot2)      # Used for graphing
library(janitor)      # Used for data cleaning

# Load the built-in iris data
data(iris)
# Install and load data.table, if you get an error where it isn't found, please install it
library(data.table)
#viewing the datatable using DT
data.table::data.table(iris)
# Load the packages, install if needed
library(plyr)
library(shiny)
library(shinythemes)
#viewing variables avalable to use
names(iris)

#making shiny UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Interactive Iris Data"),
  h1('Use the drop doen to change the graph colors and the slide the bins to change the bins'),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green", "Pink"="Pink"),
                  selected = "Blue",multiple = F),
    ),
    
  
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "lenPlot"),
      plotOutput(outputId = "widthPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output){
    
  output$lenPlot <- renderPlot({
    # Return the requested dataset ----
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }else if(input$color1=="Pink"){
      sColor = "#fb607f"
    }
    
    x    <- iris$Petal.Length
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    plots <- hist(x, breaks = bins, col = sColor, border = "white",
      xlab = "Length in CM",
      main = "Histogram Iris Petal Length Data")
    plots
  })
  
  output$widthPlot <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }else if(input$color1=="Pink"){
      sColor = "#fb607f"
    }
    
    x    <- iris$Petal.Width
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    plots2 <- hist(x, breaks = bins, col = sColor, border = "white",
      xlab = "Width in CM",
      main = "Histogram Iris Petal Width Data")
    plots2
  })
  
}

shinyApp(ui, server)








