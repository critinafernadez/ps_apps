#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

tr_Data=readRDS("final_clean_data.rds")
library(shiny)
library(ggplot2)
library(tidyverse)

list_choices_cat <-  unique(tr_Data$Category)[!is.na(unique(tr_Data$Category))]
#list_choices_type <-  unique(tr_Data$Type)[!is.na(unique(tr_Data$Type))]
list_choices_app <-  unique(tr_Data$App)[!is.na(unique(tr_Data$App))]
list_choices_type=c(unique(as.character(tr_Data$Type)[!is.na(unique(tr_Data$Type))]))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Correlation between variables"),
    sidebarLayout(position = "right",
     sidebarPanel(
       selectInput("select", label = h3("Plot by categories"), 
                   choices = character(0),
                   selected = 1),
       # selectInput("Type", label = h3("Select the type"), 
       #             choices = character(0),
       #             selected = 1)
       
       checkboxGroupInput("Type", label = h3("Plot by type"),
                     choices=list_choices_type)
       
     ), # sidebarPanel
     mainPanel(
       plotOutput(outputId = "plot", click = "plot_click"),
       tableOutput("info")
     ) # mainPanel
   ) # sidebarLayout
) # fluidPage

      


# Define server logic required to draw a histogram
# server <- function(input, output) {
#   output$plot <- renderPlot({
#     ggplot(tr_Data%>%filter(Category=="COMICS"), aes(Rating,Reviews,colour=Category))+
#       geom_point()
#   })
# }


server <- function(input, output, session) {
  
  # Can also set the label and select items
  updateSelectInput(session, "select",
                    choices = list_choices_cat,
                    selected = tail(list_choices_cat, 1)
  );
  # updateSelectInput(session, "Type",
  #                   choices = list_choices_type,
  #                   selected = tail(list_choices_type, 1)
  
  updateCheckboxGroupInput(session, "Type",
                    # choices = list_choices_type,
                    selected = tail(list_choices_type, 1)
                    
  );
  
  output$plot <- renderPlot({
    if(input$select != ""){ #& input$Type !=" "){
      # cat(file=stderr(), "input$select:", input$select == "", "\n")
      ggplot(tr_Data %>% filter(Category == input$select , Type == input$Type), aes(Rating, Reviews, colour = Category)) +
        geom_point()
    }
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)

# ggplot(tr_Data %>% filter(Category == "WEATHER" & Type == "Free"), aes(Rating, Reviews)) +
#   geom_point()
# ggplot(tr_Data %>% filter(Category == "WEATHER" & Type == "Paid"), aes(Rating, Reviews)) +
#   geom_point()
# ggplot(tr_Data %>% filter(Category == "WEATHER"), aes(Rating, Reviews)) +
#   geom_point()

library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
books <<- list("selection",label = h3("Choose a category"), 
               choices = character(0),
               selected = 1)

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(input$App %in% input$App))
    stop("Unknown book")
  
  text <- readLines(sprintf("App", input$App),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

# Text of the books downloaded from:
# A Mid Summer Night's Dream:
#  http://www.gutenberg.org/cache/epub/2242/pg2242.txt
# The Merchant of Venice:
#  http://www.gutenberg.org/cache/epub/2243/pg2243.txt
# Romeo and Juliet:
#  http://www.gutenberg.org/cache/epub/1112/pg1112.txt

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}
fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a book:",
                  choices = list_choices_cat),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
