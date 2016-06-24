library(shiny)
data(EuStockMarkets)
# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(
    
    # Give the page a title
    titlePanel("Daily Closing Prices of Major European Stock Indices,1991-1998"),
    
    # Generate a row with a sidebar
    sidebarLayout(
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("idx", "Name of Index:",
                    choices=colnames(EuStockMarkets)),
        hr(),
        helpText("Select the index above to see the plot on right hand side.")),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("plot1"),
        h4("Median"),
        verbatimTextOutput("medianVal")
      ))))