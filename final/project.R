library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
require("shinyjs")
library(corrplot)


library(RCurl)
x <- getURL("https://raw.githubusercontent.com/critinafernadez/ps_apps/master/googleplaystore.csv")
googleplaystore <- read.csv(text = x)

#googleplaystore <- read_csv("googleplaystore.csv")

#First we have a look at the variables 
class(googleplaystore)
colnames(googleplaystore)

head(googleplaystore)

#pasamos campos a factor
sapply(googleplaystore,class)
googleplaystore = googleplaystore %>% mutate(Category = as.factor(Category))%>%
  mutate(Installs = as.factor(Installs)) %>%
  mutate(Type = as.factor(Type))%>%
  mutate(`Content Rating`= as.factor(`Content.Rating`))%>%
  mutate(Genres = as.factor(Genres))%>%
  mutate(`Current Ver` = as.factor(`Current.Ver`))%>%
  mutate(`Android Ver` = as.factor(`Android.Ver`))

#quitamos la cateogria 0 de Installs, no tiene sentido.
table(googleplaystore$Installs)
googleplaystore = googleplaystore %>% filter(Installs != 0)

#NA's
sum(is.na(googleplaystore)==TRUE)
na = googleplaystore %>% filter_all(any_vars(is.na(.))) #1475 NA's
data = data.frame(lapply(googleplaystore,function(x) {
  if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))  %>% drop_na()
#we rememove the rest of missing values with drop_na
sum(is.na(data))
colSums(is.na(data))
summary(data)

#remove category NaN from variable Type
table(data$Type)
data = data[data$Type != "NaN", , drop= TRUE] %>% mutate (Type = factor(Type))

#tranformation of variable size to numeric variable measured in millions
#gsub("M","",data$Size)
sum(data$Size=="Varies with device")
#we want to remove size = "Varies with device because doesn't give us any clear information
data = data %>% filter(Size != "Varies with device") 
data = data[data$Size != "Varies with device", , drop = TRUE] %>% mutate(Size = factor(Size))

#now we take values that end in k and divide them by one million
sizem = data %>% filter(grepl("M",Size)) %>% mutate(Size = gsub("M","",Size)) %>%
  mutate(Size = as.numeric(as.character(Size)))
sizek = data %>% filter(grepl("k",Size)) %>% mutate(Size = gsub("k","", Size)) %>%
  mutate(Size = as.numeric(as.character(Size))) %>% mutate(Size = Size/1000000)

#now we have to concatenate both tables and put the numeric size into a new column
data = rbind(sizem,sizek)

#now we transform the variable price. We remove the $ symbol and mutate to numeric
head(data$Price)

data = data %>% mutate(Price = gsub("\\$","", Price)) %>% 
  mutate(Price = as.numeric(as.character(Price)))
class(data$Price)

saveRDS(data, file = "Data_clean.rds")

data1 = readRDS("Data_clean.rds")

#### Data preparation users dataset ####

x <- getURL("https://raw.githubusercontent.com/critinafernadez/ps_apps/master/googleplaystore_user_reviews.csv")
dataA=read.csv(text = x)
#data=read.csv("googleplaystore_user_reviews.csv")

# elimino la segunda columna
# quito las filas con NA

data2=dataA %>%
  drop_na() %>%
  #group_by(Sentiment)%>%
  select(-Translated_Review,-Sentiment_Polarity,-Sentiment_Subjectivity)
#columnas guya para agrupar por appsssss
valor = data2 %>% group_by(App)%>%mutate(countpos = sum(Sentiment=='Positive')) %>%
  mutate(countneut = sum(Sentiment=='Neutral')) %>% 
  mutate(counteg = sum(Sentiment=='Negative'))%>%select(-Sentiment)

valor2= valor[!duplicated(valor),]
tablefinal=valor2[-2,]


#Left join of the two datasets

data = left_join(data1, tablefinal)
saveRDS(data, "final_clean_data.rds")

##########################################################################################
vars_numerics = list(Rating = "Rating",Size ="Size")
choices_type = list(Free = "Free", Paid = "Paid")
names(choices_type) = c("Free", "Paid")

choices_category = list(Tools = "TOOLS", Family = "FAMILY", Game= "GAME",Bussiness = "BUSSINESS",LiveStyle = "LIVESTYLE")

list_choices_cat <-  unique(data$Category)[!is.na(unique(data$Category))]
list_choices_app <-  unique(data$App)[!is.na(unique(data$App))]
list_choices_type=c(unique(as.character(data$Type)[!is.na(unique(data$Type))]))
list_choices_genres <-  unique(data$Genres)[!is.na(unique(data$Genres))]
choice_bar=(data%>% select(Type,Installs,Genres,Android.Ver)%>% names())

col_scale <- scale_colour_discrete(limits = unique(data$Type))
datafree = data %>% filter(Type == "Free")
datapaid = data %>% filter(Type == "Paid")


############################################################################################

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
               tabPanel("Correlation",
                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Geom Point (Scatterplot)",
                                               fluidPage(
                                                 sidebarLayout(position = "right",
                                                               sidebarPanel(
                                                                 selectInput("select", label = h3("Plot by categories"),
                                                                             choices = character(0),
                                                                             selected = 1),
                                                                 checkboxGroupInput("Type", label = h3("Plot by type"),
                                                                                    choices=list_choices_type)
                                                               ), # sidebarPanel
                                                               mainPanel(
                                                                 plotOutput(outputId = "plot", click = "plot_click"),
                                                                 tableOutput("info")
                                                               ) # mainPanel
                                                 ) # sidebarLayout
                                               ) # fluidPage
                                      ), #  tabPanel,
                                      tabPanel("Corrplot",
                                               fluidPage(
                                                 sidebarLayout(position = "right",
                                                               sidebarPanel(
                                                                 selectInput("select2", label = h3("Plot by categories"),
                                                                             choices = character(0),
                                                                             selected = 1),
                                                                 checkboxGroupInput("Type2", label = h3("Plot by type"),
                                                                                    choices=list_choices_type)
                                                               ),
                                                               mainPanel(
                                                                 plotOutput(outputId = "plot2")
                                                               )
                                                 )))###fin de corrplot
                                      
                          ))),
               
                      
                        
                      
               tabPanel("Categorical Analysis",
                        
                        mainPanel(
                          tabsetPanel(type="tabs",
                                      tabPanel("Bar Plots",
                                               fluidPage(
                                                 sidebarLayout(position = "right",
                                                               sidebarPanel(
                                                                 selectInput("selbar", label = h3("Select a variable"),
                                                                             choices = choice_bar,selected = 1),
                                                                 selectInput("selbar2", label = h3("Select a category"),
                                                                             choices = character(0),selected = 1)
                                                                 
                                                               ),
                                                               mainPanel(
                                                                 plotOutput(outputId = "plotbar")
                                                               )
                                                               
                                                               
                                                               
                                                 )))))),
  
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
                        )   )#closes tab panel
               )

###################################################################################

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
  
  updateSelectInput(session, "select",
                    choices = list_choices_cat,
                    selected = tail(list_choices_cat, 1)
  );
  updateSelectInput(session, "select2",
                    choices = list_choices_cat,
                    selected = tail(list_choices_cat, 1)
  );
  updateSelectInput(session, "selbar",
                    choices = choice_bar
                    #selected = tail(choice_bar, 1)
  );
  
  updateSelectInput(session, "selbar2",
                    choices = list_choices_cat,
                    selected = tail(list_choices_cat, 1)
  );
  
  updateCheckboxGroupInput(session, "Type",
                           # choices = list_choices_type,
                           selected = tail(list_choices_type, 1)
                           
  );
  updateCheckboxGroupInput(session, "Type2",
                           # choices = list_choices_type,
                           selected = tail(list_choices_type, 1)
                           
  );
  
  output$plot <- renderPlot({
    if(input$select != ""){ #& input$Type !=" "){
      # cat(file=stderr(), "input$select:", input$select == "", "\n")
      ggplot(data %>% filter(Category == input$select , Type == input$Type), aes(Rating, Reviews, colour = Category)) +
        geom_point()
    }
  });
  
  output$plot2 <- renderPlot({
    if(input$select2 != "" & input$Type !=" "){ 
      M=data%>%filter(Category == input$select2 , Type == input$Type2)%>%select(Rating,Reviews,Size) %>% cor()
      corrplot(M,method = "pie")
      
    }
  })
  
  output$plotbar <- renderPlot({
    if(input$selbar=="Type"){
      ggplot(data%>% filter(Category == input$selbar2),aes(Type))+geom_bar(aes(fill=Type))}
    else if(input$selbar=="Installs"){
      ggplot(data%>% filter(Category == input$selbar2),aes(Installs))+geom_bar(aes(fill=Installs))}
    else if(input$selbar=="Genres"){
      ggplot(data%>% filter(Category == input$selbar2),aes(Genres))+geom_bar(aes(fill=Genres))}
    else{
      ggplot(data%>% filter(Category == input$selbar2),aes(Android.Ver))+geom_bar(aes(fill=Android.Ver))
    }
    
  })
  
  
  
  
  
}
shinyApp(ui = ui, server = server)
