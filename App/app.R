library(shiny)
library(tidyverse)
library(rjson)
library(shinythemes)
library(lubridate)
library(r2d3)

data <- as_data_frame(read_csv("./dane.csv"))
data$msPlayed <- data$msPlayed/(1000*3600)
data$endTime <- as.character(data$endTime)
data$endTime <- fast_strptime(data$endTime, "%Y-%m-%d %H:%M:%S",tz="UTC")
data$endTime <- as.POSIXct(data$endTime)
x1<- data %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
choices <- unique(x1$artistName)




ui <- fluidPage(theme = shinytheme("superhero"),

                sidebarLayout(
                    sidebarPanel(width=4,
                                 titlePanel("Time played"),
                                 
                                 checkboxInput("checkbox", label = "Plot by artist", value = FALSE ),
                                 
                                 uiOutput("choices") ,
                                
                                 dateInput('date',
                                           label = 'First Available Date',
                                           value = min(data$endTime)
                                 )   ,
                                 dateInput('date1',
                                           label = 'Last available Date',
                                           value = max(data$endTime)
                                          
                                 )),
                    
                    mainPanel(
                        plotOutput("distPlot",click="click")
                    )
                )
)

server <- function(input, output) {
  
  
  selected_data <- reactiveValues(
    selected = data_frame(),
    x1  = data_frame(),
    choices  = data_frame(),
    songs = data_frame()
  )
  
  observeEvent({input$date
                input$date1},{
    selected_data$selected<- data %>% filter(endTime>= input$date, endTime <= input$date1)
    selected_data$selected <- as.data.frame(selected_data$selected)
    selected_data$x1<- selected_data$selected %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% 
      arrange(desc(time)) %>% slice(1:10)
    selected_data$choices <- unique(selected_data$x1$artistName)
  })
  
  output$choices <- renderUI({
    selectInput("var", 
                label = "Choose artist to show his songs",
                choices = selected_data$choices)      
  })
  
  observeEvent(input$click,{
    if(!input$checkbox){
    lvls <- selected_data$choices
    name <- lvls[round(input$click$x)]
    browseURL(paste0("https://www.youtube.com/results?search_query=",name))
    }
  })
 
 
  output$distPlot <- renderPlot({
    
    x <- selected_data$selected %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% 
      arrange(desc(time)) %>% slice(1:10)
    y <- selected_data$selected %>% filter(artistName==input$var) %>%
      group_by(song = trackName) %>% 
      summarise(time = sum(msPlayed)) %>% 
      arrange(desc(time)) %>% slice(1:10)
    
    
    if(!input$checkbox){
      ggplot(x, aes(x=reorder(artistName,-time), y=time)) + 
        geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
        xlab(element_blank())+ylab("Hours")
    }
    
    else{
      selected_data$songs <- y
      ggplot(y, aes(x=reorder(song,-time), y=time)) + 
        geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
        xlab(element_blank())+ylab("Hours")
    }
  })
}


shinyApp(ui = ui, server = server)
