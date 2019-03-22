library(shiny)
library(shinythemes)
ui= navbarPage("GOOGLE APPS",theme = shinytheme("flatly"),
              tabPanel("Distributions",
                       mainPanel(
                         tabsetPanel(type="tabs",
                                     tabPanel("Hola"),
                                     tabPanel("Hasta luego"))
                       )),
              tabPanel("Correlation"),
              tabPanel("Categorical Analysis"),
              tabPanel("Statistical Learning"),
              tabPanel("References"))

server= function(input, output){
  
}
shinyApp(ui = ui, server = server)