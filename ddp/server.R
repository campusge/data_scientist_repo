library(shiny)
data(EuStockMarkets)
shinyServer(
  function(input, output) {
    output$plot1 <- renderPlot({
      ## Render a line chart
      plot(EuStockMarkets[,input$idx], main = input$idx, xlab = "Time", ylab = "Index Level")
      medianVal <- median(EuStockMarkets[,input$idx])
      abline(h=medianVal, col = "red")
      output$medianVal <- renderPrint(medianVal)
    })
  }
)