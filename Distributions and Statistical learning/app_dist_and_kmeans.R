
###### IMPORTANT: THE PREPARATION OF THE DATA IS IN A FILE IN OUR REPOSITORY ####
data = readRDS("final_clean_data.rds")


######## end of data preparation ###########
require(shiny)
require(tidyverse)
require("shinyjs")

#data = data %>% mutate(Price = scale(Price))
vars_numerics = list(Rating = "Rating",Size ="Size")
choices_type = list(Free = "Free", Paid = "Paid")
names(choices_type) = c("Free", "Paid")

choices_category = list(Tools = "TOOLS", Family = "FAMILY", Game= "GAME",Bussiness = "BUSSINESS",LiveStyle = "LIVESTYLE")


library(shiny)
library(shinythemes)
library(ggplot2)
ui= navbarPage("GOOGLE APPS",theme = shinytheme("flatly"),
               tabPanel("Distributions",
                        fluidPage( useShinyjs(),
                      sidebarLayout( position = "right",
                                    sidebarPanel(
                                       selectInput("var_num", label = h3("Select the variable"), 
                                                   choices = vars_numerics,
                                                   selected = 1),#closes sidebarLayout
                                    sliderInput("range", label = h3("Minimum value limit x"), min = min(data$Rating), 
                                                 max = 5, value =min(data$Price)),#c(min(data$Rating), max(data$Rating))
                                    
                                    numericInput("num", label = h3("Maximum value x axe"), value =character(0)),
                                    
                                    div(style="display:inline-block; vertical-align:top",radioButtons("type", label = h3("Type"),
                                                  choices = choices_type, 
                                                  selected = character(0))),
                                       
                                    div(style = "display:inline-block; padding-left: 40px; padding-top:15px",checkboxInput(inputId = "rug",
                                                               label = strong("Rug points"),
                                                               value = FALSE)),
                                
                                    tags$head(
                                      tags$style(HTML('#info{background-color:#465E77}'))
                                    ),
                                        div(style="display:bottom-block;vertical-align:bottom",actionButton("info", "Summary"))
                                     ), #closes sidebarPanel
                                     
                        mainPanel(
                          tabsetPanel(type="tabs",
                                      tabPanel("Densities" ,plotOutput("histPlot"),verbatimTextOutput("inform"),textOutput("note")),
                                      tabPanel("Boxplots", plotOutput("boxplot"),textOutput("notefree"),verbatimTextOutput("sumf"),
                                               textOutput("notepaid"),verbatimTextOutput("sump")))
                        )#closes mainPanel
                      )#closes sidebarlayout
                        ) #closes fluidpage
                      ),#closess tabPanel
               tabPanel("Correlation"),
               tabPanel("Categorical Analysis"),
               tabPanel("Statistical Learning",
                        pageWithSidebar(
                          headerPanel('Clustering'),
                          sidebarPanel(
                            selectInput('xcol', 'X Variable', vars_numerics),
                            selectInput('ycol', 'Y Variable', vars_numerics,
                                        selected=vars_numerics[[2]]),
                            numericInput('clusters', 'Cluster count', 3,
                                         min = 1, max = 9),
                            tags$head(
                              tags$style(HTML('#data{background-color:#465E77}'))
                            ),
                            div(style="display:bottom-block;vertical-align:bottom",actionButton("data", "Means"))
                          ),
                          mainPanel(
                            plotOutput('plot1'),
                            tableOutput("view")
                          )
                        )   ),#closes tab panel
               tabPanel("References"))

col_scale <- scale_colour_discrete(limits = unique(data$Type))
datafree = data %>% filter(Type == "Free")
datapaid = data %>% filter(Type == "Paid")
server= function(input, output, session){
  
  output$histPlot <- renderPlot(
    if(!is.null(input$type)){
    ggplot((data%>% filter(Type == input$type ) ), aes_string(x = input$var_num))+ 
      geom_histogram(aes(y = ..density..),fill = if(input$type == "Free"){"darkorchid4"}else{"red"},alpha=0.6)+ 
      theme_minimal()+
      theme(plot.title = element_text(size = 20,color= "black"))+
      geom_density(fill= if(input$type == "Free"){"darkorchid2"}else{"pink"}, alpha= 0.5,color= "darkblue")+ xlim(input$range,input$num)+ 
      ggtitle(paste("Density of",input$var_num,",","Type:", input$type,"and Category:", input$var_cat,sep=" "))+
        if(input$rug){geom_rug(aes_string(input$var_num))}

    }else{ggplot(data , aes_string(x = input$var_num))+ geom_histogram(aes(y = ..density..),fill = "blue",alpha=0.6)+ 
        theme_minimal()+
        theme(plot.title = element_text(size = 20,color= "black"))+ 
        geom_histogram(aes(y = ..density..),fill = "blue",alpha=0.6)+ 
        #ggplot(data, aes_string(x = input$var_num))+ geom_histogram(aes(y = ..density..),fill = "blue",alpha=0.6)+ theme_minimal()+  
        geom_density(fill= "lightblue", alpha= 0.5,color= "darkblue")+ xlim(input$range,input$num)+ 
        ggtitle(paste("Density of",input$var_num,",","Type:", input$type,"and Category:", input$var_cat,sep=" "))+if(input$rug){geom_rug(aes_string(input$var_num))}}
    
  )
  
  
  output$inform <- renderPrint({
    if(input$info){
      summary(data[,input$var_num])
      #paste("The mean of",input$var_num, "is:", round(mean(data[,input$var_num]),3),sep = " ")
    }
    
  })
  output$note = renderText({
    if(input$info){
      paste("Note that only selections of the variable applies to the summary")
    }
  })
  
  output$boxplot = renderPlot(
    
    ggplot(data, aes_string(x = data$Type,y = input$var_num,fill = data$Type))+ geom_boxplot(alpha = 0.7)+coord_flip()+ ylim(input$range,input$num)+
      xlab(" ")+theme_minimal()+theme(plot.title = element_text(size = 20,color= "black"))+scale_fill_brewer(palette="BuPu")+
      ggtitle(paste("Boxplot of",input$var_num,"by Type"))+
      if(input$rug){geom_rug()}
  )
  output$notefree = renderText({
    if(input$info){
      paste("Summary of",input$var_num,"for Free apps", sep = " ")
    }
  })
  output$sumf <- renderPrint({
    if(input$info){
      summary(datafree[,input$var_num])
    }
    
  })
  output$notepaid = renderText({
    if(input$info){
      paste("Summary of",input$var_num,"for Paid apps", sep = " ")
    }
  })
  
  output$sump <- renderPrint({
    if(input$info){
      summary(datapaid[,input$var_num])
    }
  })
  
  #plots from statistical learning tab
  selectedData <- reactive({
    data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  #colors.kmeans <- c("deepskyblue2","firebrick2","orange","chartreuse","blue","pink","black","green","yellow")[clusters()$cluster]
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),bty = "n",
         col = clusters()$cluster,
         pch = 21, cex = 1)
    
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$view = renderTable({
    if(input$data){
      paste("Clustering data")
      head(clusters()$centers)
    }
  })
}
shinyApp(ui = ui, server = server)