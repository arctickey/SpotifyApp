library(shiny)
library(tidyverse)
library(rjson)
library(lubridate)
library(r2d3)

data <- as_data_frame(read_csv("./dane.csv"))
data$msPlayed <- data$msPlayed/(1000*3600)
data$endTime <- as.character(data$endTime)
data$endTime <- fast_strptime(data$endTime, "%Y-%m-%d %H:%M:%S",tz="UTC")
data$endTime <- as.POSIXct(data$endTime)
x <- data %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
choices <- unique(x$artistName)


ui <- fluidPage(
    
  
    sidebarLayout(
      
        
        sidebarPanel(width=4,
                     titlePanel("Time played"),
                     
                     checkboxInput("checkbox", label = "Plot by artist", value = FALSE ),
                     
                     hr(),
                     fluidRow(column(3, verbatimTextOutput("value"))),
                     
                     selectInput("var", 
                                 label = "Choose artist to show his songs",
                                 choices = choices,
                                 selected = "Arctic Monkeys"),      
                dateInput('date',
                          label = 'First Available Date',
                          value = min(data$endTime)
                )   ,
                dateInput('date2',
                          label = 'Last available Date',
                          value = max(data$endTime)
                          
                          
        )),
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        data1 <- data %>% filter(endTime>= input$date, endTime <= input$date2)
        
        
        if(input$checkbox==TRUE){
            x <- data1 %>% filter(artistName == input$var) %>% group_by(song = trackName) %>% 
                summarise(time = sum(msPlayed)) %>% 
                arrange(desc(time)) %>% slice(1:10)
            ggplot(x, aes(x=reorder(song,-time), y=time)) + 
                geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
                theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
                xlab(element_blank())+ylab("Hours")
        }
        else{
            x <- data1 %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
            ggplot(x, aes(x=reorder(artistName,-time), y=time)) + 
                geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
                theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
                xlab(element_blank())+ylab("Hours")
        }
     

    })
}



shinyApp(ui = ui, server = server)
